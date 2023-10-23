use chrono::Local;
use log2::{debug, info};
use nalgebra::{matrix, vector, Vector6};
use plotters::prelude::*;
use std::error::Error;
use utils::{
    lab1::{__GenericSquareMatrix, __GenericVector, gradient_method, new_positive_definite_matrix},
    FloatingType,
};

const MIN: FloatingType = -10.0;
const MAX: FloatingType = 10.0;
const LAMBDA: FloatingType = 0.0001;
const EPSILON: FloatingType = 0.00001;
const PLOT_DIR: &str = "plots";

fn main() -> Result<(), Box<dyn Error>> {
    let _log2 = log2::open("output.log").start();

    let a = new_positive_definite_matrix::<6>(MIN, MAX);
    let b = (MAX - MIN) * Vector6::new_random() + Vector6::from_element(MIN);
    let x0 = (MAX - MIN) * Vector6::new_random() + Vector6::from_element(MIN);

    //// CASE FOR REPORT
    // let a = matrix![
    //     65.11405470521025 ,-26.762746171102414, -22.007698068539373,  -82.77671290257723,   35.00621520974893,   75.58190587265923;
    //    -26.762746171102414,   229.0660173609204,   19.93232989043198,  11.317673881705893,   -41.5887940396087,  27.507719995937514;
    //    -22.007698068539373,   19.93232989043198,   47.85586622454126,   92.30117607044637,  -20.18791540235534, -18.150124329594377;
    //     -82.77671290257723,  11.317673881705893,   92.30117607044637,  231.24867418816547, -30.569264277267344, -103.31843363394694;
    //     35.00621520974893,   -41.5887940396087,  -20.18791540235534, -30.569264277267344,   130.1539779623694,   82.94972766849409 ;
    //      75.58190587265923,  27.507719995937514, -18.150124329594377, -103.31843363394694,   82.94972766849409,   189.0452850784499
    // ];
    // let b = vector![
    //     9.814255243856422,
    //     -0.3400832138148502,
    //     -4.2742798826246275,
    //     0.5729798470675629,
    //     1.0452978781511018,
    //     -6.258477409122025,
    // ];
    // let x0 = vector![
    //     -6.998099039133139,
    //     -5.532602553195874,
    //     5.222094920523634,
    //     3.71860843139695,
    //     -5.88821505753657,
    //     4.175398532986163,
    // ];

    let function = Function::new(a, b);

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
    a: __GenericSquareMatrix<N>,
    b: __GenericVector<N>,
}

impl<const N: usize> Function<N> {
    fn new(a: __GenericSquareMatrix<N>, b: __GenericVector<N>) -> Self {
        Self { a, b }
    }

    pub fn f(&self, x: &__GenericVector<N>) -> FloatingType {
        (x.transpose() * self.a * x)[(0, 0)] / 2 as FloatingType + self.b.dotc(x)
    }
    pub fn f_prime(&self, x: &__GenericVector<N>) -> __GenericVector<N> {
        (self.a + self.a.transpose()) * x / 2 as FloatingType + self.b
    }

    pub fn f_prime_inv(&self, f_prime_val: __GenericVector<N>) -> __GenericVector<N> {
        2 as FloatingType
            * (self.a.transpose() + self.a).try_inverse().unwrap()
            * (f_prime_val - self.b)
    }
}

fn plot_steps(f_x_steps: Vec<FloatingType>) -> Result<(), Box<dyn Error>> {
    let path = format!(
        "{}/{}.png",
        PLOT_DIR,
        Local::now().format("%Y-%m-%d %H:%M:%S")
    );

    debug!("Generating plot at \'{}\'", path);

    let root = BitMapBackend::new(path.as_str(), (900, 750)).into_drawing_area();
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
    let x_rg = -(f_x_steps.len() as FloatingType * 0.05) as i64
        ..(f_x_steps.len() as FloatingType * 1.05) as i64;
    let y_rg = y_min - 0.05 * y_max.abs()..y_max + 0.05 * y_max.abs();

    let mut chart = ChartBuilder::on(&root)
        .caption(
            "Зависимость значения функции от номера шага методом градиентного спуска",
            ("sans-serif", 22).into_font(),
        )
        .margin(10)
        .x_label_area_size(50)
        .y_label_area_size(70)
        .build_cartesian_2d(x_rg, y_rg)?;

    chart
        .configure_mesh()
        .x_desc("i [шаг]")
        .y_desc("f(xi)")
        .draw()?;

    let ls = LineSeries::new(
        f_x_steps.iter().enumerate().map(|(i, &f)| (i as i64, f)),
        RED,
    );

    chart
        .draw_series(ls)?
        .label("f(xi)")
        .legend(|(x, y)| PathElement::new(vec![(x, y), (x + 20, y)], RED));

    chart
        .configure_series_labels()
        .background_style(WHITE.mix(0.8))
        .border_style(BLACK)
        .draw()?;

    root.present()?;

    Ok(())
}
