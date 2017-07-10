extern crate render1;
extern crate gl;
extern crate cgmath;

use render1::Object;
use render1::Segment;
use cgmath::Matrix4;
use cgmath::SquareMatrix;
use cgmath::Vector2;
use cgmath::Point2;
use cgmath::Vector3;
use cgmath::InnerSpace;
use cgmath::EuclideanSpace;
use cgmath::Deg;
use cgmath::Basis2;
use cgmath::Rotation2;
use cgmath::Rotation;

#[derive(Debug, Copy, Clone)]
struct Ray {
    x0: Vector2<f32>,
    direction: Vector2<f32>,
}

impl Ray {
    fn object(&self, color: &'static str) -> Object<Vec<[f32; 2]>> {
        Object::segment(vec![self.x0.into(), (self.x0 + self.direction.normalize_to(40.0)).into()], color)
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

    fn unwrap_point(&self) -> Vector2<f32> {
        match *self {
            Intersection::Point(p) => p,
            _ => panic!()
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
    } else if a.cross(b) != 0.0 && t_a >= 0.0 && t_b >= 0.0 && t_b <= 1.0 {
        Intersection::Point(a0 + a * t_a)
    } else {
        Intersection::None
    }
}

fn segment_segment_intersection(a: Segment, b: Segment) -> Intersection {
    let a0 = a.x0;
    let a = a.x;
    let b0 = b.x0;
    let b = b.x;
    let t_a = (b0 - a0).cross(b) / a.cross(b);
    let t_b = (b0 - a0).cross(a) / a.cross(b);
    let zero_to_one = Interval {
        left: Endpoint::inclusive(0.0),
        right: Endpoint::inclusive(1.0)
    };

    if a.cross(b) == 0.0 && (b0 - a0).cross(a) == 0.0 {
        let t0 = (b0 - a0).dot(a) / a.dot(a);
        let t1 = t0 + b.dot(a) / a.dot(a);
        let (t0, t1) = if a.dot(b) > 0.0 { (t0, t1) } else { (t1, t0) };
        let t0_to_t1 = Interval {
            left: Endpoint::inclusive(t0),
            right: Endpoint::inclusive(t1),
        };
        match t0_to_t1.intersection(zero_to_one) {
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
    } else if a.cross(b) != 0.0 && t_a >= 0.0  && t_a <= 1.0 && t_b >= 0.0 && t_b <= 1.0 {
        Intersection::Point(a0 + a * t_a)
    } else {
        Intersection::None
    }
}

fn segments(vertices: &[[f32; 2]]) -> Vec<Segment> {
    let mut buf = Vec::new();
    for i in 0..vertices.len() - 1 {
        if let Some(pair) = vertices.get(i..i + 2) {
            let p0: Vector2<f32> = pair[0].into();
            let p1: Vector2<f32> = pair[1].into();
            buf.push(Segment { x0: p0, x: p1 - p0 });
        } else {
            return buf
        }
    }
    buf
}

fn between_rays(left: Ray, right: Ray, point: Vector2<f32>) -> bool {
    debug_assert!(left.direction.normalize() == right.direction.normalize() && (left.x0 - right.x0).dot(left.direction) == 0.0);
    let side1 = {
        let point = point - left.x0;
        point.cross(left.direction).signum()
    };
    let side2 = {
        let point = point - right.x0;
        point.cross(right.direction).signum()
    };
    side1 != side2 && point.dot(left.direction) > 0.0
}

fn vector2_projection(v: Vector2<f32>, onto: Vector2<f32>) -> Vector2<f32> {
    onto.normalize_to(v.dot(onto))
}

fn extension_points(midline: Ray, left: Ray, right: Ray, segments: &[Segment]) -> Vec<Vector2<f32>> {
    debug_assert!(left.direction.normalize() == right.direction.normalize() && (left.x0 - right.x0).dot(left.direction) == 0.0);
    let mut points = Vec::new();
    for &segment in segments {
        let mut intersection = false;
        for &ray in &[left, right] {
            match ray_segment_intersection(ray, segment) {
                Intersection::None => continue,
                Intersection::Point(p) => {
                    points.push(p);
                }
                Intersection::Segment(s) => {
                    points.push(s.x0);
                    points.push(s.x0 + s.x);
                }
            }
            intersection = true;
        }
        for &p in &[segment.x0, segment.x0 + segment.x] {
            if between_rays(left, right, p) {
                points.push(p);
            }
        }
    }
    points
}

// returns [left, mid, right]
fn left_mid_right_rays(left: Vector2<f32>, right: Vector2<f32>) -> (Ray, Ray, Ray) {
    let direction = right - left;
    let normal = Vector2::new(-direction.y, direction.x);
    let mid = Ray { x0: Point2::from_vec(left).midpoint(Point2::from_vec(right)).to_vec(), direction: normal };
    let left = Ray { x0: left, direction: normal };
    let right = Ray { x0: right, direction: normal };
    (left, mid, right)
}

macro_rules! points {
    (values { $($name:ident = $value:expr);+; } points [$($x:ident $y:ident);+]) => ({
            $(let $name: f32 = $value;)+
            vec![$([$x, $y]),+]
    })
}

use std::env;

fn main() {
    let angle: f32 = env::args().nth(1).unwrap().parse().unwrap();
    let angle = Deg(angle);

    let mut objects = vec![];

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
    objects.push(Object::outline(board_outline.clone(), "white"));


    //let aabb_segments = render1::AABB::from(&board_outline).segments();
    let outline_segments = segments(&board_outline);

    let midpoint = [15.0, 12.0];
    let width = 5.0;
    let rotator = Basis2::from_angle(angle);
    let left = Vector2::from(midpoint) + rotator.rotate_vector(Vector2::new(width, 0.0));
    let right = Vector2::from(midpoint) + rotator.rotate_vector(Vector2::new(-width, 0.0));
    let segment = Segment { x0: left.into(), x: Vector2::from(right) - Vector2::from(left) };
    objects.push(segment.object("green"));
    objects.push(Object::point(left.into(), "blue"));
    objects.push(Object::point(right.into(), "red"));

    fn extended_segment(left: Vector2<f32>, right: Vector2<f32>, outline: &[Segment], objects: &mut Vec<Object<Vec<[f32; 2]>>>) -> Option<Segment> {
        let (left, mid, right) = left_mid_right_rays(left.into(), right.into());
        let mut points = extension_points(mid, left, right, &outline);
        points.sort_by(|p1, p2| (p1 - mid.x0).dot(mid.direction.normalize()).partial_cmp(&(p2 - mid.x0).dot(mid.direction.normalize())).unwrap());
        'p: for &p in points.iter() {
            let segment_point = p - mid.direction.normalize_to((p - mid.x0).dot(mid.direction.normalize()));
            let translation = mid.direction.normalize_to((p - mid.x0).dot(mid.direction.normalize()) + 0.1);
            let extended = Segment { x0: left.x0 + translation, x: right.x0 - left.x0 };
            let left_extended = Segment { x0: left.x0, x: extended.x0 - left.x0 };
            let right_extended = Segment { x0: right.x0, x: extended.x0 + extended.x - right.x0 };
            let bottom = Segment { x0: left.x0, x: right.x0 - left.x0 };
            let normal_segment = Segment { x0: segment_point, x: p - segment_point };
            objects.push(extended.object("pink"));
            objects.push(left_extended.object("pink"));
            objects.push(right_extended.object("pink"));
            objects.push(bottom.object("pink"));
            let mut intersect_count = (0, 0);
            for &segment in outline {
                match segment_segment_intersection(segment, left_extended) {
                    Intersection::None => {}
                    _ => {
                        intersect_count.0 += 1;
                    }
                }
                match segment_segment_intersection(segment, right_extended) {
                    Intersection::None => {}
                    _ => {
                        intersect_count.1 += 1;
                    }
                }
                match segment_segment_intersection(segment, extended) {
                    Intersection::None => {}
                    _ => continue 'p,
                }
                // also check if extended is still inside the polygon?
            }
            println!("intersect count: {:?}", intersect_count);
            if intersect_count.0 % 2 == 0 || intersect_count.1 % 2 == 0 {
                continue
            }
            // return closest segment extension that satisfies conditions above
            return Some(extended)
        }
        None
    }

    let extended_segment = extended_segment(left.into(), right.into(), &outline_segments, &mut objects).unwrap();
    objects.push(extended_segment.object("blue"));

    render1::render1(&objects);
}
