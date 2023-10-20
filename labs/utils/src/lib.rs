pub type FloatingType = f64;

pub mod lab1 {
    use nalgebra::{ArrayStorage, Const, DimMin, SquareMatrix, Vector};

    use crate::FloatingType;

    type _SquareMatrixN<const N: usize> =
        SquareMatrix<FloatingType, Const<N>, ArrayStorage<FloatingType, N, N>>;
    type _VectorN<const N: usize> =
        Vector<FloatingType, Const<N>, ArrayStorage<FloatingType, N, 1>>;

    pub fn new_positive_definite_matrix<const N: usize>(
        min: FloatingType,
        max: FloatingType,
    ) -> _SquareMatrixN<N>
    where
        Const<N>: DimMin<Const<N>, Output = Const<N>>,
    {
        loop {
            let m = (max - min) * _SquareMatrixN::<N>::new_random()
                + _SquareMatrixN::<N>::from_element(min);
            if m.determinant() > 0 as FloatingType {
                return m.transpose() * m;
            }
        }
    }

    pub fn gradient_method<const N: usize, F>(
        x0: &_VectorN<N>,
        lambda: FloatingType,
        epsilon: FloatingType,
        f_prime: F,
    ) -> Vec<_VectorN<N>>
    where
        F: Fn(&_VectorN<N>) -> _VectorN<N>,
    {
        let mut x_log = vec![*x0];
        loop {
            let x = x_log.last().unwrap();
            let x_next = x - lambda * f_prime(x);
            if (x_next - x).norm() < epsilon {
                break;
            } else {
                x_log.push(x_next)
            }
        }
        x_log
    }
}
