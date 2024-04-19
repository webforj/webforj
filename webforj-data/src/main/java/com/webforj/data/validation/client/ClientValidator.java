package com.webforj.data.validation.client;

/**
 * Represents a client-side validator.
 *
 * <p>
 * A client-side validator is a validator that can be used to validate a component on the client
 * side. The validation is typically done by the client-side JavaScript code, which can be used to
 * provide immediate feedback to the user. This interface provides a way to define the validation
 * logic that will be executed on the client side.
 * </p>
 *
 * @since 24.01
 * @author Hyyan Abo Fakher
 */
@FunctionalInterface
public interface ClientValidator {

  /**
   * Returns the expression to be used for client-side validation.
   *
   * @return the expression.
   */
  public String getExpression();

  /**
   * Creates a new client validator with the given expression.
   *
   * @param expression the expression.
   * @param args the arguments to be used in the expression.
   *
   * @return the client validator.
   */
  public static ClientValidator of(String expression, Object... args) {
    return () -> String.format(expression, args);
  }
}
