package com.webforj.automation.see;

import java.util.ServiceLoader;
import java.util.Set;

/**
 * Serializes the components that implement a given contract into snapshot nodes.
 *
 * <p>
 * webforJ provides serializers for its concerns and components. Custom serializers are registered
 * through {@link ServiceLoader} in
 * {@code META-INF/services/com.webforj.automation.see.SnapshotSerializer}. They run after the
 * serializers of webforJ, ordered by contract name, and can override the attributes those set. Two
 * custom serializers for the same contract, or two writing the same attribute of a node, are
 * rejected.
 * </p>
 *
 * <p>
 * Custom serializers always run. They read state held on the client only when
 * {@link SnapshotNode#isBrowserState()} returns true.
 * </p>
 *
 * @param <C> the type of the contract, a concern or a component class
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public interface SnapshotSerializer<C> {

  /**
   * Returns the contract this serializer handles, a concern or a component class.
   *
   * @return the contract
   */
  Class<C> getContract();

  /**
   * Serializes the given component into its node.
   *
   * @param target the component
   * @param node the node of the component
   */
  default void describe(C target, SnapshotNode node) {}

  /**
   * Returns the contracts whose state is held on the server for the components this serializer
   * handles.
   *
   * <p>
   * Some concerns are held on the client for most components. A component whose state of such a
   * concern is read from the server returns that concern here, so the concern is serialized even
   * when the snapshot does not include the browser state.
   * </p>
   *
   * @return the contracts, empty by default
   */
  default Set<Class<?>> getServerHeldContracts() {
    return Set.of();
  }

  /**
   * Returns the contracts whose state is held on the client for the components this serializer
   * handles.
   *
   * <p>
   * Concerns are serialized for every component that implements them, and most of their state is
   * held on the server. A component whose state of a concern is read from the client returns that
   * concern here, so the concern is serialized only when the snapshot includes the browser state.
   * </p>
   *
   * @return the contracts, empty by default
   */
  default Set<Class<?>> getClientHeldContracts() {
    return Set.of();
  }
}
