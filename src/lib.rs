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
use std::borrow::Borrow;

#[derive(Debug, Copy, Clone)]
pub struct AABB {
    pub min: Vector2<f32>,
    pub max: Vector2<f32>,
}

impl AABB {
    pub fn object(&self, color: &'static str) -> Object<Vec<[f32; 2]>> {
        let p0 = [self.min.x, self.min.y];
        let p1 = [self.max.x, self.min.y];
        let p2 = [self.max.x, self.max.y];
        let p3 = [self.min.x, self.max.y];
        let vertices = vec![p0, p1, p2, p3, p0];
        Object::outline(vertices, color)
    }

    pub fn segments(&self) -> Vec<Segment> {
        let p0: Vector2<f32> = [self.min.x, self.min.y].into();
        let p1: Vector2<f32> = [self.max.x, self.min.y].into();
        let p2: Vector2<f32> = [self.max.x, self.max.y].into();
        let p3: Vector2<f32> = [self.min.x, self.max.y].into();
        vec![
            Segment { x0: p0.into(), x: (p1 - p0).into() },
            Segment { x0: p0.into(), x: (p3 - p0).into() },
            Segment { x0: p2.into(), x: (p1 - p2).into() },
            Segment { x0: p2.into(), x: (p3 - p2).into() },
        ]
    }
}

impl std::ops::BitOr for AABB {
    type Output = AABB;
    fn bitor(self, rhs: AABB) -> Self::Output {
        let min = Vector2::new(self.min.x.min(rhs.min.x), self.min.y.min(rhs.min.y));
        let max = Vector2::new(self.max.x.max(rhs.max.x), self.max.y.max(rhs.max.y));
        AABB {
            min,
            max,
        }
    }
}

impl<T: AsRef<[[f32; 2]]>> From<T> for AABB {
    fn from(vertices: T) -> AABB {
        let vertices = vertices.as_ref();
        if vertices.is_empty() {
            panic!("Can't create AABB for zero-length list of vertices");
        }
        let mut min = vertices[0];
        let mut max = vertices[0];
        for vertex in &vertices[1..] {
            min[0] = min[0].min(vertex[0]);
            max[0] = max[0].max(vertex[0]);
            min[1] = min[1].min(vertex[1]);
            max[1] = max[1].max(vertex[1]);
        }
        AABB {
            min: min.into(),
            max: max.into(),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Segment {
    pub x0: Vector2<f32>,
    pub x: Vector2<f32>,
}

impl Segment {
    pub fn object(&self, color: &'static str) -> Object<Vec<[f32; 2]>> {
        Object::segment(vec![self.x0.into(), (self.x0 + self.x).into()], color)
    }
}

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
    look_around_on: bool,
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

const CAMERA_SPEED: f32 = 0.1;
const INITIAL_CAMERA_SENSITIVITY: f32 = 0.009;

impl Camera {
    pub fn new<V: Into<Vector3<f32>>>(position: V, look_at: V, up: V) -> Camera {
        let position = position.into();
        let forward = look_at.into() - position;
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
            look_around_on: false,
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
            S => &mut self.moving.downward,
            D => &mut self.moving.right,
            C if state == Pressed => {
                println!("look_around: {}", if self.look_around_on { "on" } else { "off" });
                self.look_around_on = !self.look_around_on;
                return;
            }
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
            Space => &mut self.moving.upward,
            Up => &mut self.moving.upward,
            Left => &mut self.moving.left,
            Right => &mut self.moving.right,
            Down => &mut self.moving.downward,
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
        if self.look_around_on {
            self.forward = Vector3 {
                x: self.yaw.cos() * self.pitch.cos(),
                y: self.pitch.sin(),
                z: self.yaw.sin() * self.pitch.cos(),
            }.normalize();
        }
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
        if self.moving.upward {
            self.position += Vector3::unit_y() * CAMERA_SPEED;
        }
        if self.moving.downward {
            self.position -= Vector3::unit_y() * CAMERA_SPEED;
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
#[repr(C)]
pub struct RawObject {
    vertices_ptr: *const [f32; 2],
    vertices_len: usize,
    primitive: u32,
    color: *const i8,
    model: *const [f32; 16],
}

#[derive(Debug, Clone)]
pub struct Object<T: Clone + Borrow<[[f32; 2]]>> {
    pub vertices: T,
    pub primitive: gl::types::GLenum,
    pub color: &'static str,
    pub model: Matrix4<f32>,
}

impl<T: Clone + Borrow<[[f32; 2]]>> Object<T> {
    /*
    pub fn from_raw(raw: &RawObject) -> Object<T> {
        unsafe {
            let vertices = std::slice::from_raw_parts(raw.vertices_ptr, raw.vertices_len);
            let color = std::ffi::CStr::from_ptr(raw.color).to_str().unwrap();
            let model: &Matrix4<f32> = From::from(&*raw.model);
            Object {
                vertices,
                primitive: raw.primitive,
                color,
                model: *model,
            }
        }
    }
    */

    pub fn segment(vertices: T, color: &'static str) -> Object<T> {
        Object {
            vertices,
            primitive: gl::LINE_STRIP,
            color,
            model: Matrix4::identity(),
        }
    }

    pub fn outline(vertices: T, color: &'static str) -> Object<T> {
        Object {
            vertices,
            primitive: gl::LINE_STRIP,
            color,
            model: Matrix4::identity(),
        }
    }
}

impl Object<Vec<[f32; 2]>> {
    pub fn point(point: [f32; 2], color: &'static str) -> Object<Vec<[f32; 2]>> {
        let dx = 0.1;
        let p0 = [point[0] - dx, point[1] - dx];
        let p1 = [point[0] + dx, point[1] - dx];
        let p2 = [point[0] + dx, point[1] + dx];
        let p3 = [point[0] - dx, point[1] + dx];
        let vertices = vec![p0, p1, p2, p3, p0, p2];
        Object {
            vertices,
            primitive: gl::TRIANGLES,
            color,
            model: Matrix4::identity()
        }
    }
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
        "purple" => [0.5, 0.0, 0.5, 1.0],
        "pink" => [1.0, 0.75, 0.796, 1.0],
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

    pub fn create_targets<T: Clone + Borrow<[[f32; 2]]>>(&mut self, objects: &[Object<T>]) -> Vec<Target> {
        let vertices: Vec<[f32; 2]> = objects.iter().fold(Vec::new(), |mut buf, obj| { buf.extend(obj.vertices.borrow()); buf });
        self.buffer_data(&vertices);
        objects.borrow().iter().fold((0, Vec::new()), |(offset, mut buf), obj| {
            let target = Target {
                primitive: obj.primitive,
                color: color(obj.color),
                buffer_offset: offset,
                vertex_count: obj.vertices.borrow().len(),
                model: obj.model,
            };
            buf.push(target);
            (offset + obj.vertices.borrow().len(), buf)
        }).1
    }
}

/*
#[no_mangle]
pub extern fn render1<'a>(ptr: *const RawObject, len: usize) {
    let raw_objects = unsafe {
        std::slice::from_raw_parts(ptr, len)
    };
    println!("{:?}", raw_objects);
    let objects = raw_objects.iter().map(|raw| {
        Object::from_raw(raw)
    }).collect::<Vec<Object<'a>>>();
    println!("{:?}", objects);
*/
//pub fn render1<T: Clone + Borrow<[[f32; 2]]>>(objects: &[Object<T>]) {
pub fn render1(objects: &[Object<Vec<[f32; 2]>>]) {
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

    let aabb = objects.iter().fold(None, |aabb, obj| {
        aabb.map(|aabb| {
            Some(aabb | AABB::from(&obj.vertices))
        }).unwrap_or(Some(AABB::from(&obj.vertices)))
    }).unwrap();

    let aabb = {
        let mut aabb = aabb;
        aabb.min -= [0.1, 0.1].into();
        aabb.max += [0.1, 0.1].into();
        aabb
    };

    use cgmath::Point3;
    let center: Point3<f32> = [aabb.min.x + (aabb.max.x - aabb.min.x) / 2.0, aabb.min.y + (aabb.max.y - aabb.min.y) / 2.0, 0.0].into();
    let eye_z = 0.5 * (aabb.max.x - aabb.min.x) / f32::to_radians(45.0 * 0.5).tan() / (width as f32 / height as f32);
    //println!("{:?}", eye_z);
    let proj = cgmath::perspective(cgmath::Deg(45.0), width as f32 / height as f32, 0.1, 100.0);
    let mut camera = Camera::new((center + Vector3::new(0.0, 0.0, eye_z)).to_vec(), center.to_vec(), [0.0, 1.0, 0.0].into());

    let mut objects = objects.to_vec();
    //objects.push(aabb.object("red"));
    let objects = &objects;

    let (targets, vao, vbo) = unsafe {
        let mut vao = 0;
        let mut vbo = 0;
        gl::GenVertexArrays(1, &mut vao);
        gl::BindVertexArray(vao);

        gl::GenBuffers(1, &mut vbo);
        gl::BindBuffer(gl::ARRAY_BUFFER, vbo);

        let mut vertex_buffer = VertexBuffer::new(vbo);
        let targets = vertex_buffer.create_targets(&objects);

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
        //println!("{:?}", target.model);
        //println!("DrawArrays({}, offset={}, count={})", target.primitive, target.buffer_offset as i32, target.vertex_count as i32);
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
