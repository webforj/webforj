package com.webforj.automation.see;

import com.webforj.automation.find.ComponentTree;
import com.webforj.component.Component;
import com.webforj.concern.HasVisibility;
import java.util.Collections;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Builds the node tree of a snapshot. Each component becomes a node serialized by the serializers
 * of the registry, in their order.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
final class SnapshotRenderer {
  private final SnapshotSerializerRegistry serializers;
  private final int maxWidth;
  private final boolean browserState;
  private final boolean hiddenComponents;
  private final boolean composites;
  private final Map<SnapshotNode, Map<String, SnapshotSerializer<?>>> applicationWrites =
      new IdentityHashMap<>();
  private SnapshotSerializer<?> running;

  SnapshotRenderer(SnapshotSerializerRegistry serializers, SnapshotOptions options) {
    this.serializers = serializers;
    this.maxWidth = options.getMaxWidth();
    this.browserState = options.isBrowserState();
    this.hiddenComponents = options.isHiddenComponents();
    this.composites = options.isComposites();
  }

  SnapshotNode describe(Component component) {
    SnapshotNode root = build(component, null);
    apply(root, Collections.newSetFromMap(new IdentityHashMap<>()));

    return root;
  }

  int getMaxWidth() {
    return maxWidth;
  }

  boolean isBrowserState() {
    return browserState;
  }

  boolean isComposites() {
    return composites;
  }

  /**
   * Checks if the given component is included in the snapshot. A hidden component is included only
   * when hidden components are included. A component whose visibility is held on the client is
   * included only when the snapshot includes the browser state.
   *
   * @param component the component
   * @return true if the component is included
   */
  boolean isShown(Component component) {
    if (hiddenComponents || !(component instanceof HasVisibility<?> visibility)) {
      return true;
    }

    if (!browserState && serializers.isContractHeldByClient(HasVisibility.class, component)) {
      return true;
    }

    try {
      return visibility.isVisible();
    } catch (UnsupportedOperationException e) {
      return true;
    }
  }

  void recordWrite(SnapshotNode node, String key) {
    if (running == null) {
      return;
    }

    Map<String, SnapshotSerializer<?>> writes =
        applicationWrites.computeIfAbsent(node, ignored -> new HashMap<>());
    SnapshotSerializer<?> previous = writes.putIfAbsent(key, running);
    if (previous != null && previous != running) {
      throw new IllegalStateException("The snapshot serializers " + previous.getClass().getName()
          + " and " + running.getClass().getName() + " both write <" + key + "> on "
          + node.getType() + ". One of them has to own it.");
    }
  }

  private SnapshotNode build(Component component, SnapshotNode parent) {
    SnapshotNode node =
        new SnapshotNode(component.getClass().getSimpleName(), component, this, parent);
    for (Component child : ComponentTree.getChildren(component)) {
      if (isShown(child)) {
        node.getChildren().add(build(child, node));
      }
    }

    return node;
  }

  private void apply(SnapshotNode node, Set<SnapshotNode> described) {
    if (!described.add(node)) {
      return;
    }

    Component component = node.getComponent();
    for (SnapshotSerializer<?> serializer : serializers.forComponent(component)) {
      if (browserState || !serializers.isBrowserStateRequired(serializer, component)) {
        running = serializers.isApplicationSerializer(serializer) ? serializer : null;
        apply(serializer, component, node);
      }
    }

    running = null;
    for (SnapshotNode child : List.copyOf(node.getChildren())) {
      apply(child, described);
    }
  }

  private static <C> void apply(SnapshotSerializer<C> serializer, Component component,
      SnapshotNode node) {
    serializer.describe(serializer.getContract().cast(component), node);
  }
}
