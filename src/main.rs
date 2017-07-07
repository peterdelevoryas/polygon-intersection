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
use cgmath::SquareMatrix;

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
pub struct Object<'a> {
    vertices: &'a [[f32; 2]],
    primitive: gl::types::GLenum,
    color: &'a str,
    model: Matrix4<f32>,
}

#[derive(Debug)]
pub struct Target {
    pub primitive: gl::types::GLenum,
    pub color: [f32; 4],
    pub buffer_offset: usize,
    pub vertex_count: usize,
    pub model: Matrix4<f32>,
}


pub fn color(color: &str) -> [f32; 4] {
    match color {
        "red" => [1.0, 0.0, 0.0, 1.0],
        "blue" => [0.0, 0.0, 1.0, 1.0],
        "green" => [0.0, 1.0, 0.0, 1.0],
        "white" => [1.0, 1.0, 1.0, 1.0],
        "black" => [0.0, 0.0, 0.0, 1.0],
        _ => panic!("unknown color: {:?}", color)
    }
}

#[derive(Debug)]
pub struct VertexBuffer {
    handle: gl::types::GLuint,
}

impl VertexBuffer {
    pub fn new(handle: gl::types::GLuint) -> VertexBuffer {
        VertexBuffer { handle }
    }

    /// Returns offset in buffer
    pub fn buffer_data(&mut self, vertices: &[[f32; 2]]) {
        unsafe {
            gl::BufferData(
                gl::ARRAY_BUFFER,
                (vertices.len() * 2 * std::mem::size_of::<f32>()) as _,
                std::mem::transmute(vertices.as_ptr()),
                gl::STATIC_DRAW);
        }
    }

    pub fn create_targets<'a>(&mut self, objects: &[Object<'a>]) -> Vec<Target> {
        let vertices: Vec<[f32; 2]> = objects.iter().fold(Vec::new(), |mut buf, obj| { buf.extend(obj.vertices); buf });
        self.buffer_data(&vertices);
        objects.iter().fold((0, Vec::new()), |(offset, mut buf), obj| {
            let target = Target {
                primitive: obj.primitive,
                color: color(obj.color),
                buffer_offset: offset,
                vertex_count: obj.vertices.len(),
                model: obj.model,
            };
            buf.push(target);
            (offset + obj.vertices.len(), buf)
        }).1
    }
}

fn render(objects: &[Object]) {
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

    let proj = cgmath::perspective(cgmath::Deg(45.0), width as f32 / height as f32, 0.1, 100.0);
    let mut camera = Camera::new([0.0, 0.0, 1.0], [0.0, 0.0, 0.0], [0.0, 1.0, 0.0]);

    let (targets, vao, vbo) = unsafe {
        let mut vao = 0;
        let mut vbo = 0;
        gl::GenVertexArrays(1, &mut vao);
        gl::BindVertexArray(vao);

        gl::GenBuffers(1, &mut vbo);
        gl::BindBuffer(gl::ARRAY_BUFFER, vbo);

        let mut vertex_buffer = VertexBuffer::new(vbo);
        let targets = vertex_buffer.create_targets(objects);

        let position = gl::GetAttribLocation(program, std::ffi::CString::new("position").unwrap().as_ptr());
        gl::EnableVertexAttribArray(position as _);
        gl::VertexAttribPointer(position as _, 2, gl::FLOAT, gl::FALSE as _, 0, std::ptr::null());

        gl::UseProgram(program);
        gl::BindFragDataLocation(program, 0, std::ffi::CString::new("out_color").unwrap().as_ptr());

        (targets, vao, vbo)
    };

    let transform_location = unsafe {
        gl::GetUniformLocation(program, std::ffi::CString::new("transform").unwrap().as_ptr())
    };

    let color_location = unsafe {
        gl::GetUniformLocation(program, std::ffi::CString::new("color").unwrap().as_ptr())
    };

    for target in &targets {
        println!("DrawArrays({}, offset={}, count={})", target.primitive, target.buffer_offset as i32, target.vertex_count as i32);
    }

    use glutin::Event::WindowEvent;
    use glutin::WindowEvent::Closed;
    use glutin::WindowEvent::KeyboardInput;
    use glutin::WindowEvent::MouseMoved;
    use glutin::WindowEvent::MouseLeft;
    use glutin::VirtualKeyCode::Escape;

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
            gl::ClearColor(0.3, 0.3, 0.3, 1.0);
            gl::Clear(gl::COLOR_BUFFER_BIT);

            for target in &targets {
                let transform = proj * camera.view() * target.model;
                gl::UniformMatrix4fv(transform_location, 1, gl::FALSE as _, transform.as_ptr());
                gl::Uniform4fv(color_location, 1, target.color.as_ptr());
                gl::DrawArrays(target.primitive, target.buffer_offset as _, target.vertex_count as _);
            }

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

fn main() {
    let triangle: [[f32; 2]; 4] = [
        [ 0.0,  0.5],
        [ 0.5, -0.5],
        [-0.5, -0.5],
        [ 0.0,  0.5],
    ];
    let triangle = Object {
        vertices: &triangle[..],
        primitive: gl::LINE_STRIP,
        color: "red",
        model: Matrix4::identity(),
    };

    let square: [[f32; 2]; 5] = [
        [-0.5, -0.5],
        [-0.5,  0.5],
        [ 0.5,  0.5],
        [ 0.5, -0.5],
        [-0.5, -0.5],
    ];
    let square = Object {
        vertices: &square[..],
        primitive: gl::LINE_STRIP,
        color: "blue",
        model: Matrix4::identity(),
    };

    let objects = &[triangle, square];
    render(objects);
}
