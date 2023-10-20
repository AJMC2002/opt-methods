use log2::info;
use nalgebra::Vector6;
use plotters::prelude::*;
use std::error::Error;
use utils::{
    lab1::{_SquareMatrixN, _VectorN, gradient_method, new_positive_definite_matrix},
    FloatingType,
};

const MIN: FloatingType = -10.0;
const MAX: FloatingType = 10.0;
const LAMBDA: FloatingType = 0.0001;
const EPSILON: FloatingType = 0.00001;

fn main() -> Result<(), Box<dyn Error>> {
    let _log2 = log2::open("output.log").start();

    let a = new_positive_definite_matrix::<6>(MIN, MAX);
    let b = (MAX - MIN) * Vector6::new_random() + Vector6::from_element(MIN);
    let function = Function::new(a, b);

    let x0 = (MAX - MIN) * Vector6::new_random() + Vector6::from_element(MIN);
    let x_steps = gradient_method(&x0, LAMBDA, EPSILON, |x| function.f_prime(x));
    let x_exact = function.f_prime_inv(Vector6::zeros());

    let steps = x_steps.len() - 1;
    let intermediate_results = [
        x_steps[0],
        x_steps[steps / 4],
        x_steps[steps / 2],
        x_steps[3 * steps / 4],
        x_steps[steps],
    ];

    info!("a {}", a);
    info!("b {}", b);
    info!("x0 {}", x0);
    info!("tochnoe {}", x_exact);
    info!("xm {}", x_steps.last().unwrap());
    info!("m (steps) {}", steps);
    info!(
        "intermediate steps\n{}",
        intermediate_results
            .iter()
            .enumerate()
            .map(|(i, it)| format!("x_{{{}m/{}}}\n{}", i, intermediate_results.len() - 1, it))
            .collect::<Vec<String>>()
            .join("")
    );
    info!("x (exact solution) {}", x_exact);
    info!(
        "f(intermediate steps)\n{}",
        intermediate_results
            .iter()
            .enumerate()
            .map(|(i, it)| format!(
                "f(x_{{{}m/{}}}) = {}\n",
                i,
                intermediate_results.len() - 1,
                function.f(it)
            ))
            .collect::<Vec<String>>()
            .join("")
    );
    info!("f(x) (exact solution) = {}", function.f(&x_exact));
    info!(
        "abs diff x vector = {}",
        (x_steps.last().unwrap() - x_exact).map(FloatingType::abs)
    );
    info!(
        "abs diff f(x) = {}",
        (function.f(x_steps.last().unwrap()) - function.f(&x_exact)).abs()
    );

    plot_steps(
        x_steps
            .iter()
            .map(|x| function.f(x))
            .collect::<Vec<FloatingType>>(),
    )?;

    Ok(())
}

struct Function<const N: usize> {
    a: _SquareMatrixN<N>,
    b: _VectorN<N>,
}

impl<const N: usize> Function<N> {
    fn new(a: _SquareMatrixN<N>, b: _VectorN<N>) -> Self {
        Self { a, b }
    }

    pub fn f(&self, x: &_VectorN<N>) -> FloatingType {
        (x.transpose() * self.a * x)[(0, 0)] / 2 as FloatingType + self.b.dotc(x)
    }
    pub fn f_prime(&self, x: &_VectorN<N>) -> _VectorN<N> {
        (self.a + self.a.transpose()) * x / 2 as FloatingType + self.b
    }

    pub fn f_prime_inv(&self, f_prime_val: _VectorN<N>) -> _VectorN<N> {
        2 as FloatingType
            * (self.a.transpose() + self.a).try_inverse().unwrap()
            * (f_prime_val - self.b)
    }
}

fn plot_steps(f_x_steps: Vec<FloatingType>) -> Result<(), Box<dyn Error>> {
    let root = BitMapBackend::new(&"plots/0.png", (640, 480)).into_drawing_area();
    root.fill(&WHITE)?;
    let (y_min, y_max) = (
        *f_x_steps
            .iter()
            .min_by(|a, b| a.partial_cmp(b).unwrap())
            .unwrap(),
        *f_x_steps
            .iter()
            .max_by(|a, b| a.partial_cmp(b).unwrap())
            .unwrap(),
    );
    let mut chart = ChartBuilder::on(&root)
        .caption(
            "Зависимости значения функции от номера шага методом градиентного спуска",
            ("sans-serif", 50).into_font(),
        )
        .margin(5)
        .x_label_area_size(30)
        .y_label_area_size(30)
        .build_cartesian_2d(0..f_x_steps.len(), y_min..y_max)?;

    chart.configure_mesh().draw()?;

    let ls = LineSeries::new(f_x_steps.iter().enumerate().map(|(i, f)| (i, *f)), &RED);
    chart
        .draw_series(ls)?
        .label("f(x)")
        .legend(|(x, y)| PathElement::new(vec![(x, y), (x + 100, y)], RED));

    chart
        .configure_series_labels()
        .background_style(WHITE.mix(0.8))
        .border_style(BLACK)
        .draw()?;

    root.present()?;

    Ok(())
}
