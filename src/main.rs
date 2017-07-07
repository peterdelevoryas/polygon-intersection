extern crate gl;
extern crate glutin;
extern crate cgmath;
extern crate winit;

use cgmath::Vector2;
use cgmath::Vector3;
use cgmath::Matrix4;
use cgmath::Matrix;
use cgmath::Point2;
use cgmath::Point3;
use cgmath::EuclideanSpace;
use cgmath::InnerSpace;
use cgmath::Rad;
use cgmath::Deg;
use cgmath::Angle;

#[derive(Debug)]
pub struct Camera {
    position: Vector3<f32>,
    forward: Vector3<f32>,
    up: Vector3<f32>,
    moving: Moving,
    last_mouse_position: Option<Vector2<f32>>,
    yaw: Rad<f32>,
    pitch: Rad<f32>,
    sensitivity: f32,
}

#[derive(Debug)]
pub struct Moving {
    forward: bool,
    backward: bool,
    left: bool,
    right: bool,
    upward: bool,
    downward: bool,
}

impl Moving {
    pub fn new() -> Moving {
        Moving {
            forward: false,
            backward: false,
            left: false,
            right: false,
            upward: false,
            downward: false,
        }
    }
}

use glutin::ElementState;
use glutin::VirtualKeyCode;
use winit::ModifiersState;

const CAMERA_SPEED: f32 = 0.05;
const INITIAL_CAMERA_SENSITIVITY: f32 = 0.009;

impl Camera {
    pub fn new<V: Into<Vector3<f32>>>(position: V, look_at: V, up: V) -> Camera {
        let position = position.into();
        let forward = (look_at.into() - position).normalize();
        let up = up.into();
        Camera {
            position,
            forward,
            up,
            moving: Moving::new(),
            last_mouse_position: None,
            yaw: Rad(0.0),
            pitch: Rad(0.0),
            sensitivity: INITIAL_CAMERA_SENSITIVITY,
        }
    }

    pub fn view(&self) -> Matrix4<f32> {
        Matrix4::look_at(Point3::from_vec(self.position),
                         Point3::from_vec(self.position + self.forward),
                         self.up)
    }

    fn xz(&self) -> Vector3<f32> {
        Vector3::new(self.forward.x, 0.0, self.forward.z).normalize_to(1.0)
    }

    pub fn handle_keyboard_input(&mut self, state: ElementState, code: VirtualKeyCode, modifiers: ModifiersState) {
        use VirtualKeyCode::*;
        use ElementState::*;
        let moving = match code {
            W => &mut self.moving.forward,
            A => &mut self.moving.left,
            S => &mut self.moving.backward,
            D => &mut self.moving.right,
            Subtract if modifiers.shift && state == Pressed => {
                self.sensitivity -= 0.01 * INITIAL_CAMERA_SENSITIVITY;
                println!("decreased sensitivity to {}", self.sensitivity);
                return
            }
            Equals if modifiers.shift && state == Pressed => {
                self.sensitivity += 0.01 * INITIAL_CAMERA_SENSITIVITY;
                println!("increased sensitivity to {}", self.sensitivity);
                return
            }
            _ => return
        };
        if !*moving && state == Pressed {
            *moving = true;
        } else if *moving && state == Released {
            *moving = false;
        }
    }

    pub fn handle_mouse_moved(&mut self, position: Vector2<f32>) {
        let diff = match self.last_mouse_position {
            Some(old_position) => (position - old_position) * self.sensitivity,
            None => Vector2::new(0.0, 0.0),
        };
        self.last_mouse_position = Some(position);
        self.yaw += Rad(diff.x);
        self.pitch -= Rad(diff.y);
        if self.pitch > Deg(89.0).into() {
            self.pitch = Deg(89.0).into();
        } else if self.pitch < Deg(-89.0).into() {
            self.pitch = Deg(-89.0).into();
        }
        self.forward = Vector3 {
            x: self.yaw.cos() * self.pitch.cos(),
            y: self.pitch.sin(),
            z: self.yaw.sin() * self.pitch.cos(),
        }.normalize();
    }

    pub fn move1(&mut self) {
        if self.moving.forward {
            self.position += self.xz() * CAMERA_SPEED;
        }
        if self.moving.backward {
            self.position += -self.xz() * CAMERA_SPEED;
        }
        if self.moving.left {
            self.position += Vector3::unit_y().cross(self.xz()).normalize_to(CAMERA_SPEED);
        }
        if self.moving.right {
            self.position += self.xz().cross(Vector3::unit_y()).normalize_to(CAMERA_SPEED);
        }
    }
}

fn shader_compile_status(shader: gl::types::GLuint) -> Result<(), String> {
    unsafe {
        let mut status = gl::FALSE as _;
        gl::GetShaderiv(shader, gl::COMPILE_STATUS, &mut status);
        if status != gl::TRUE as _ {
            let len = {
                let mut size = 0;
                gl::GetShaderiv(shader, gl::INFO_LOG_LENGTH, &mut size);
                size as usize - 1
            };
            let mut buf = Vec::with_capacity(len);
            buf.set_len(len);
            gl::GetShaderInfoLog(shader, len as _, std::ptr::null_mut(), buf.as_mut_ptr() as _);
            let message = String::from_utf8_unchecked(buf);
            Err(message)
        } else {
            Ok(())
        }
    }
}

fn compile_shader(source: &str, kind: gl::types::GLenum) -> Result<gl::types::GLuint, String> {
    unsafe {
        let shader = gl::CreateShader(kind);
        let ptr = source.as_ptr() as *const _;
        let len = [source.len() as _];
        gl::ShaderSource(shader, 1, &ptr, len.as_ptr());
        gl::CompileShader(shader);
        shader_compile_status(shader)?;
        Ok(shader)
    }
}

fn program_link_status(program: gl::types::GLuint) -> Result<(), String> {
    unsafe {
        let mut status = gl::FALSE as _;
        gl::GetProgramiv(program, gl::LINK_STATUS, &mut status);
        if status != gl::TRUE as _ {
            let len = {
                let mut size = 0;
                gl::GetProgramiv(program, gl::INFO_LOG_LENGTH, &mut size);
                size as usize - 1
            };
            let mut buf = Vec::with_capacity(len);
            buf.set_len(len);
            gl::GetProgramInfoLog(program, len as _, std::ptr::null_mut(), buf.as_mut_ptr() as _);
            let message = String::from_utf8_unchecked(buf);
            Err(message)
        } else {
            Ok(())
        }
    }
}

fn link_program(shaders: &[gl::types::GLuint]) -> Result<gl::types::GLuint, String> {
    unsafe {
        let program = gl::CreateProgram();
        for &shader in shaders {
            gl::AttachShader(program, shader);
        }
        gl::LinkProgram(program);
        program_link_status(program)?;
        Ok(program)
    }
}

#[derive(Debug)]
pub struct Shape {
    primitive: gl::types::GLenum,
    color: [f32; 4],
    vao: gl::types::GLuint,
    vbo: gl::types::GLuint,
}

impl Shape {
    pub fn new(vertices: &[Point2<f32>], primitive: gl::types::GLenum, color: [u8; 4]) -> Shape {
        let color = [color[0] as f32 / 255.0,
                     color[1] as f32 / 255.0,
                     color[2] as f32 / 255.0,
                     color[3] as f32 / 255.0];
        let (vao, vbo) = unsafe {
            let mut vao = 0;
            let mut vbo = 0;
            gl::GenVertexArrays(1, &mut vao);
            gl::BindVertexArray(vao);
            gl::GenBuffers(1, &mut vbo);
            gl::BindBuffer(gl::ARRAY_BUFFER, vbo);
            gl::BufferData(
                gl::ARRAY_BUFFER,
                (vertices.len() * std::mem::size_of::<gl::types::GLfloat>()) as _,
                std::mem::transmute(vertices.as_ptr()),
                gl::STATIC_DRAW);
            gl::BindVertexArray(0);
        };

        Shape {
            primitive,
            color,
        }
    }
}

fn main() {
    let width = 1024;
    let height = 768;

    let events_loop = glutin::EventsLoop::new();
    let window = glutin::WindowBuilder::new()
        .with_title("render1")
        .with_dimensions(width, height)
        .with_vsync()
        .build(&events_loop)
        .unwrap();

    window.set_cursor_state(glutin::CursorState::Hide);

    unsafe {
        window.make_current().unwrap();
        gl::load_with(|s| window.get_proc_address(s) as *const _);
    }


    let vs = compile_shader(include_str!("vertex.glsl"), gl::VERTEX_SHADER).unwrap();
    let fs = compile_shader(include_str!("fragment.glsl"), gl::FRAGMENT_SHADER).unwrap();
    let program = link_program(&[vs, fs]).unwrap();

    let triangle_point_list: [gl::types::GLfloat; 6] = [0.0, 0.5, 0.5, -0.5, -0.5, -0.5];
    let square_point_list: [gl::types::GLfloat; 8] = [-0.5, -0.5, 0.5, -0.5, 0.5, 0.5, -0.5, 0.5];
    let proj = cgmath::perspective(cgmath::Deg(45.0), width as f32 / height as f32, 0.1, 100.0);
    let mut camera = Camera::new([0.0, 0.0, 1.0], [0.0, 0.0, 0.0], [0.0, 1.0, 0.0]);

    let position = gl::GetAttribLocation(program, std::ffi::CString::new("position").unwrap().as_ptr());
    gl::EnableVertexAttribArray(position as _);
    gl::VertexAttribPointer(position as _, 2, gl::FLOAT, gl::FALSE as _, 0, std::ptr::null());

    let (vao, vbo) = unsafe {
        let mut vao = 0;
        let mut vbo = 0;
        gl::GenVertexArrays(1, &mut vao);
        gl::BindVertexArray(vao);

        gl::GenBuffers(1, &mut vbo);
        gl::BindBuffer(gl::ARRAY_BUFFER, vbo);
        gl::BufferData(
            gl::ARRAY_BUFFER,
            (triangle_point_list.len() * std::mem::size_of::<gl::types::GLfloat>()) as _,
            std::mem::transmute(triangle_point_list.as_ptr()),
            gl::STATIC_DRAW);
        gl::UseProgram(program);
        gl::BindFragDataLocation(program, 0, std::ffi::CString::new("out_color").unwrap().as_ptr());

        (vao, vbo)
    };

    let (vao2, vbo2) = unsafe {
        let mut vao = 0;
        let mut vbo = 0;
        gl::GenVertexArrays(1, &mut vao);
        gl::BindVertexArray(vao);

        gl::GenBuffers(1, &mut vbo);
        gl::BindBuffer(gl::ARRAY_BUFFER, vbo);
        gl::BufferData(
            gl::ARRAY_BUFFER,
            (square_point_list.len() * std::mem::size_of::<gl::types::GLfloat>()) as _,
            std::mem::transmute(square_point_list.as_ptr()),
            gl::STATIC_DRAW);

        (vao, vbo)
    };


    use glutin::Event::WindowEvent;
    use glutin::WindowEvent::Closed;
    use glutin::WindowEvent::KeyboardInput;
    use glutin::WindowEvent::MouseMoved;
    use glutin::WindowEvent::MouseLeft;
    use glutin::VirtualKeyCode::Escape;

    let transform_location = unsafe {
        gl::GetUniformLocation(program, std::ffi::CString::new("transform").unwrap().as_ptr())
    };
    let mut running = true;
    while running {
        events_loop.poll_events(|event| {
            let WindowEvent { event, ..} = event;
            match event {
                Closed => running = false,
                KeyboardInput(_, _, Some(Escape), _) => {
                    running = false;
                }
                KeyboardInput(key_state, _, Some(key_code), modifiers) => {
                    camera.handle_keyboard_input(key_state, key_code, modifiers);
                }
                MouseMoved(x, y) => {
                    camera.handle_mouse_moved(Vector2 { x: x as _, y: y as _ });
                }
                MouseLeft => {
                    camera.last_mouse_position = None;
                }
                _ => {}
            }
        });

        camera.move1();

        unsafe {
            let transform = proj * camera.view();
            gl::UniformMatrix4fv(transform_location, 1, gl::FALSE as _, transform.as_ptr());

            gl::ClearColor(0.3, 0.3, 0.3, 1.0);
            gl::Clear(gl::COLOR_BUFFER_BIT);
            gl::DrawArrays(gl::TRIANGLES, 0, 3);

            window.swap_buffers().unwrap();
        }
    }

    unsafe {
        gl::DeleteProgram(program);
        gl::DeleteShader(vs);
        gl::DeleteShader(fs);
        gl::DeleteBuffers(1, &vbo);
        gl::DeleteVertexArrays(1, &vao);
    }

    println!("window closed");
}
