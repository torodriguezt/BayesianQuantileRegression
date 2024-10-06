import tensorflow as tf


def validate_inputs(y: tf.Tensor, mu: tf.Tensor, sigma: tf.Tensor, tau: tf.Tensor) -> None:
    """
    Validates the inputs to the skew_double_exponential_lpdf function.
    """
    tf.debugging.assert_all_finite(mu, message="Location parameter mu must be finite.")
    tf.debugging.assert_positive(sigma, message="Scale parameter sigma must be positive and finite.")
    tf.debugging.assert_greater_equal(tau, 0.0, message="Skewness parameter tau must be in the range [0, 1].")
    tf.debugging.assert_less_equal(tau, 1.0, message="Skewness parameter tau must be in the range [0, 1].")

    if tf.reduce_any(tf.math.is_nan(y)) or tf.reduce_any(tf.math.is_nan(mu)):
        raise ValueError("Random variable 'y' or location parameter 'mu' contains NaN values.")

def skew_double_exponential_lpdf(
    y: tf.Tensor, mu: tf.Tensor, sigma: tf.Tensor, tau: tf.Tensor
) -> tuple:
    """
    Computes the log probability density function (log-PDF) of the skew double exponential distribution
    and its gradients with respect to the input parameters.
    
    Parameters:
    y (tf.Tensor): Random variable(s).
    mu (tf.Tensor): Location parameter.
    sigma (tf.Tensor): Scale parameter, must be positive.
    tau (tf.Tensor): Skewness parameter, must be between 0 and 1.
    
    Returns:
    tuple: Log-PDF value and gradients with respect to y, mu, sigma, and tau.
    """
    validate_inputs(y, mu, sigma, tau)

    inv_sigma = 1.0 / sigma
    y_m_mu = y - mu
    sign_diff = tf.sign(y_m_mu)

    is_negative_diff = tf.cast(sign_diff < 0, dtype=tf.float32)
    abs_diff_y_mu = tf.abs(y_m_mu)
    abs_diff_y_mu_scaled = abs_diff_y_mu * inv_sigma

    expo = (is_negative_diff + sign_diff * tau) * abs_diff_y_mu_scaled

    logp = -2.0 * expo
    logp += tf.math.log(2.0)
    logp -= tf.math.log(sigma)
    logp += tf.math.log(tau) + tf.math.log1p(-tau)

    deriv = 2.0 * (is_negative_diff + sign_diff * tau) * sign_diff * inv_sigma

    grad_y = -deriv
    grad_mu = deriv
    grad_sigma = -inv_sigma + 2.0 * expo * inv_sigma
    grad_tau = 1.0 / tau - 1.0 / (1.0 - tau) - sign_diff * 2.0 * abs_diff_y_mu_scaled

    return logp, grad_y, grad_mu, grad_sigma, grad_tau


if __name__ == "__main__":
    y = tf.constant([1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0], dtype=tf.float32)
    mu = tf.constant(0.5, dtype=tf.float32)
    sigma = tf.constant(1.2, dtype=tf.float32)
    tau = tf.constant(0.5, dtype=tf.float32)

    logp, grad_y, grad_mu, grad_sigma, grad_tau = skew_double_exponential_lpdf(y, mu, sigma, tau)

    print(f"logp: {logp.numpy()}")
    print(f"grad_y: {grad_y.numpy()}")
    print(f"grad_mu: {grad_mu.numpy()}")
    print(f"grad_sigma: {grad_sigma.numpy()}")
    print(f"grad_tau: {grad_tau.numpy()}")
