extern crate render1;
extern crate gl;
extern crate cgmath;

use render1::Object;
use cgmath::Matrix4;
use cgmath::SquareMatrix;
use cgmath::Vector2;
use cgmath::Vector3;
use cgmath::InnerSpace;

#[derive(Debug, Copy, Clone)]
struct Segment {
    x0: Vector2<f32>,
    x: Vector2<f32>,
}

impl Segment {
    fn object(&self, color: &'static str) -> Object<Vec<[f32; 2]>> {
        Object::segment(vec![self.x0.into(), (self.x0 + self.x).into()], color)
    }
}

#[derive(Debug, Copy, Clone)]
struct Ray {
    x0: Vector2<f32>,
    direction: Vector2<f32>,
}

impl Ray {
    fn object(&self, color: &'static str) -> Object<Vec<[f32; 2]>> {
        Object::segment(vec![self.x0.into(), (self.x0 + self.direction * 1_000_000_000.0).into()], color)
    }
}

#[derive(Debug, Copy, Clone)]
enum Intersection {
    None,
    Point(Vector2<f32>),
    Segment(Segment),
}

impl Intersection {
    fn object(&self, color: &'static str) -> Option<Object<Vec<[f32; 2]>>> {
        match *self {
            Intersection::None => None,
            Intersection::Point(p) => Some(Object::point(p.into(), color)),
            Intersection::Segment(s) => Some(Object::segment(vec![s.x0.into(), (s.x0 + s.x).into()], color)),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub struct Interval {
    left: Endpoint,
    right: Endpoint,
}

enum IntervalIntersection {
    None,
    Point(f32),
    Interval(Interval),
}

fn point_or_interval_intersection(left: Endpoint, right: Endpoint) -> IntervalIntersection {
    if left == right && left.inclusive && right.inclusive {
        IntervalIntersection::Point(left.value)
    } else {
        IntervalIntersection::Interval(Interval { left, right })
    }
}

impl Interval {
    fn intersection(self, rhs: Interval) -> IntervalIntersection {
        let a = self.left;
        let b = self.right;
        let x = rhs.left;
        let y = rhs.right;

        if a <= b && b <= x && x <= y {
            if b == x {
                IntervalIntersection::Point(b.value)
            } else {
                IntervalIntersection::None
            }
        } else if a <= x && x <= b && b <= y {
            point_or_interval_intersection(x, b)
        } else if a <= x && x <= y && y <= b {
            point_or_interval_intersection(x, y)
        } else if x <= a && a <= b && b <= y {
            point_or_interval_intersection(a, b)
        } else if x <= a && a <= y && y <= b {
            point_or_interval_intersection(a, y)
        } else if x <= y && y <= a && a <= b {
            if y == a {
                IntervalIntersection::Point(y.value)
            } else {
                IntervalIntersection::None
            }
        } else {
            unreachable!()
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Endpoint {
    value: f32,
    inclusive: bool,
}

impl Endpoint {
    fn inclusive(value: f32) -> Endpoint {
        Endpoint { value, inclusive: true }
    }
    fn exclusive(value: f32) -> Endpoint {
        Endpoint { value, inclusive: false }
    }
}

use std::cmp::PartialEq;
use std::cmp::PartialOrd;
use std::cmp::Ordering;
impl PartialEq for Endpoint {
    fn eq(&self, other: &Endpoint) -> bool {
        self.value == other.value
    }
}
impl PartialOrd for Endpoint {
    fn partial_cmp(&self, other: &Endpoint) -> Option<Ordering> {
        if self.value < other.value {
            Some(Ordering::Less)
        } else if self.value == other.value {
            Some(Ordering::Equal)
        } else if self.value > other.value {
            Some(Ordering::Greater)
        } else {
            None
        }
    }
}

trait Cross2 {
    type Scalar;
    fn cross(self, rhs: Self) -> Self::Scalar;
}

impl Cross2 for Vector2<f32> {
    type Scalar = f32;
    fn cross(self, rhs: Self) -> f32 {
        self.x * rhs.y - self.y * rhs.x
    }
}

fn ray_segment_intersection(ray: Ray, segment: Segment) -> Intersection {
    let a0 = ray.x0;
    let a = ray.direction;
    let b0 = segment.x0;
    let b = segment.x;
    let t_a = (b0 - a0).cross(b) / a.cross(b);
    let t_b = (b0 - a0).cross(a) / a.cross(b);
    let zero_to_inf = Interval {
        left: Endpoint::inclusive(0.0),
        right: Endpoint::exclusive(std::f32::INFINITY)
    };

    if a.cross(b) == 0.0 && (b0 - a0).cross(a) == 0.0 {
        let t0 = (b0 - a0).dot(a) / a.dot(a);
        let t1 = t0 + b.dot(a) / a.dot(a);
        let (t0, t1) = if a.dot(b) > 0.0 { (t0, t1) } else { (t1, t0) };
        let t0_to_t1 = Interval {
            left: Endpoint::inclusive(t0),
            right: Endpoint::inclusive(t1),
        };
        match t0_to_t1.intersection(zero_to_inf) {
            IntervalIntersection::None => Intersection::None,
            IntervalIntersection::Point(p) => {
                Intersection::Point(a0 + a * p)
            }
            IntervalIntersection::Interval(i) => {
                let p0 = a0 + a * i.left.value;
                let p1 = a0 + a * i.right.value;
                Intersection::Segment(Segment {
                    x0: p0,
                    x: p1 - p0,
                })
            }
        }
    } else if a.cross(b) == 0.0 && (b0 - a0).cross(a) != 0.0 {
        Intersection::None
    } else if a.cross(b) != 0.0 && t_a >= 0.0 && t_b >= 0.0 {
        Intersection::Point(a0 + a * t_a)
    } else {
        Intersection::None
    }
}

macro_rules! points {
    (values { $($name:ident = $value:expr);+; } points [$($x:ident $y:ident);+]) => ({
            $(let $name: f32 = $value;)+
            vec![$([$x, $y]),+]
    })
}

fn main() {
    let board_outline = points! {
        values {
            x1 = 60.969119;
            y1 = 0.0000000;

            x2 = 60.969119;
            y2 = 21.999237;

            x3 = 36.000000;
            y3 = 22.000000;

            x4 = 36.000000;
            y4 = 14.500000;

            x5 = 24.999877;
            y5 = 14.500000;

            x6 = 25.000000;
            y6 = 18.500000;

            x7 =  0.000000;
            y7 = 18.500000;

            x8 = 0.000000;
            y8 = 3.500000;

            x9 = 25.000000;
            y9 = 3.500000;

            x10 = 24.999877;
            y10 = 7.500111;

            x11 = 36.000000;
            y11 = 7.500111;

            x12 = 36.000000;
            y12 = 0.000000;

            x13 = 60.969119;
            y13 = 0.000000;
        }
        points [x1 y1; x2 y2; x3 y3; x4 y4; x5 y5; x6 y6;
                x7 y7; x8 y8; x9 y9; x10 y10; x11 y11; x12 y12;
                x13 y13]
    };

    let top_segment = Object::segment(vec![[5.0, 5.0], [10.0, 7.5]], "green");
        

    let objects = &[Object::outline(board_outline, "white"), top_segment];

    render1::render1(objects);
}
