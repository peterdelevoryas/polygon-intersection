extern crate gl;
extern crate glutin;
extern crate cgmath;

use cgmath::Vector3;
use cgmath::Vector2;

#[derive(Debug)]
pub struct Camera {
    position: Vector3<f32>,
    forward: Vector3<f32>,
    up: Vector3<f32>,
}

impl Camera {
    pub fn new(position: Vector3<f32>, look_at: Vector3<f32>, up: Vector3<f32>) -> Camera {
        Camera {
            position,
            forward: look_at - position,
            up,
        }
    }
}

#[derive(Debug)]
pub struct State {
    camera: Camera,
}

impl State {
    pub fn new(camera: Camera) -> Self {
        Self {
            camera,
        }
    }

    pub fn move_camera(&mut self) {
    }
}

#[derive(Debug)]
pub struct Runner {}

#[derive(Debug, Copy, Clone)]
pub enum Run {
    Continue,
    Stop,
}

impl Runner {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run<F>(&self, mut frame_callback: F)
    where
        F: FnMut() -> Run
    {
        while let Run::Continue = frame_callback() {}
    }
}

fn main() {
    let events_loop = glutin::EventsLoop::new();
    let window = glutin::WindowBuilder::new()
        .with_title("render1")
        .with_dimensions(1024, 768)
        .with_vsync()
        .build(&events_loop)
        .unwrap();

    let camera = Camera::new(
        [0.0, 0.0, 1.0].into(),
        [0.0, 0.0, 0.0].into(),
        [0.0, 1.0, 0.0].into());
    let runner = Runner::new();
    let mut state = State::new(camera);

    unsafe {
        window.make_current().unwrap();

        gl::load_with(|s| window.get_proc_address(s) as *const _);
        gl::ClearColor(0.0, 0.0, 0.0, 1.0);

        runner.run(move || {
            events_loop.poll_events(|event| {
                state.move_camera();
            });
            Run::Continue
        });
    }
}
