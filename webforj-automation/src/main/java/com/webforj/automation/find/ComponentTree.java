package com.webforj.automation.find;

import com.webforj.App;
import com.webforj.automation.see.Snapshot;
import com.webforj.automation.see.SnapshotOptions;
import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;
import com.webforj.component.Composite;
import com.webforj.concern.HasComponents;
import com.webforj.concern.HasPrefix;
import com.webforj.concern.HasSuffix;
import java.util.ArrayList;
import java.util.Collections;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Set;
import java.util.function.Consumer;
import java.util.function.Supplier;

/**
 * Walks and prints the component tree of the context under test.
 *
 * <p>
 * The tree is rooted at the frames of the application under test. Composite components expose the
 * same underlying instances through more than one path, so the walk visits every component exactly
 * once.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class ComponentTree {

  private ComponentTree() {}

  /**
   * Returns the roots of the component tree, the frames of the application under test.
   *
   * @return the roots, in creation order
   */
  public static List<Component> getRoots() {
    return List.copyOf(App.getFrames());
  }

  /**
   * Walks the tree below the given root and passes every component to the visitor.
   *
   * <p>
   * The root itself is visited first. Every component is visited exactly once even when it is
   * reachable through more than one path.
   * </p>
   *
   * @param root the component to start from
   * @param visitor the visitor receiving every component
   */
  public static void walk(Component root, Consumer<Component> visitor) {
    visit(root, visitor, Collections.newSetFromMap(new IdentityHashMap<>()));
  }

  /**
   * Returns the children of the given component.
   *
   * @param component the component
   *
   * @return the children, never null
   */
  public static List<Component> getChildren(Component component) {
    List<Component> children = new ArrayList<>();
    if (component instanceof Composite<?>) {
      children.addAll(getChildren(ComponentUtil.getBoundComponent(component)));
    }

    if (component instanceof HasComponents container) {
      try {
        children.addAll(container.getComponents());
      } catch (UnsupportedOperationException e) {
        // the component declares HasComponents but its bound component holds no children
      }
    }

    if (component instanceof HasPrefix<?> prefix) {
      addSlot(children, prefix::getPrefixComponent);
    }

    if (component instanceof HasSuffix<?> suffix) {
      addSlot(children, suffix::getSuffixComponent);
    }

    // a container keeps naming a component it holds after the component was destroyed, and a
    // composite names the children its bound component names
    children.removeIf(Component::isDestroyed);
    Set<Component> once = Collections.newSetFromMap(new IdentityHashMap<>());
    children.removeIf(child -> !once.add(child));

    return withoutWrapped(children);
  }

  /**
   * Returns a printable representation of the tree below the given root.
   *
   * @param root the component to start from
   *
   * @return the tree as text, one component per line
   */
  public static String render(Component root) {
    return Snapshot.render(root, new SnapshotOptions().setComposites(true));
  }

  /**
   * Returns a printable representation of the whole component tree of the context.
   *
   * @return the printed tree
   */
  public static String renderAll() {
    List<Component> roots = getRoots();
    if (roots.isEmpty()) {
      return "<no frames>";
    }

    StringBuilder output = new StringBuilder();
    for (Component root : roots) {
      output.append(Snapshot.render(root, new SnapshotOptions().setComposites(true)));
    }

    return output.toString();
  }

  private static void visit(Component component, Consumer<Component> visitor,
      Set<Component> visited) {
    if (component == null || component.isDestroyed() || !visited.add(component)) {
      return;
    }

    visitor.accept(component);
    for (Component child : getChildren(component)) {
      visit(child, visitor, visited);
    }
  }

  private static void addSlot(List<Component> children, Supplier<Component> slot) {
    try {
      Component child = slot.get();
      if (child != null) {
        children.add(child);
      }
    } catch (UnsupportedOperationException e) {
      // the component declares the slot but its bound component has none
    }
  }

  // a window registers the bound component of every composite it shows, so its list names a card
  // and the element of that card side by side. The element belongs under the card only.
  private static List<Component> withoutWrapped(List<Component> children) {
    Set<Component> wrapped = Collections.newSetFromMap(new IdentityHashMap<>());
    Set<Component> seen = Collections.newSetFromMap(new IdentityHashMap<>());
    for (Component child : children) {
      collectWrapped(child, wrapped, seen);
    }

    return children.stream().filter(child -> !wrapped.contains(child)).toList();
  }

  // walks the raw children, the filtered walk would cut the branch this method is deciding on
  private static void collectWrapped(Component component, Set<Component> wrapped,
      Set<Component> seen) {
    if (!seen.add(component)) {
      return;
    }

    if (component instanceof Composite<?>) {
      Component bound = ComponentUtil.getBoundComponent(component);
      wrapped.add(bound);
      collectWrapped(bound, wrapped, seen);
    }

    if (component instanceof HasComponents container) {
      try {
        for (Component child : container.getComponents()) {
          collectWrapped(child, wrapped, seen);
        }
      } catch (UnsupportedOperationException e) {
        // the component declares HasComponents but its bound component holds no children
      }
    }
  }
}
