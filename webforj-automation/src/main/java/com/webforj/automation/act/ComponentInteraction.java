package com.webforj.automation.act;

/**
 * What a user can do with the components of a contract.
 *
 * <p>
 * Every verb of {@link UserInteraction} has an interface extending this one. An implementation
 * names the contract of the components it serves and is registered in the {@code META-INF/services}
 * entry of this interface. The implementations of the application are asked before the shipped
 * ones. One implementation of the application serves a component for a verb, and two that serve the
 * same component are rejected.
 * </p>
 *
 * @param <C> the contract of the components the interaction serves
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public interface ComponentInteraction<C> {

  /**
   * Returns the contract of the components this interaction serves.
   *
   * @return the class or interface the components implement
   */
  Class<C> getContract();

  /**
   * Reports whether this interaction serves the given component.
   *
   * @param component the component, an instance of the contract
   *
   * @return true when the interaction serves the component
   */
  default boolean isSupported(C component) {
    return true;
  }
}
