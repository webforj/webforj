package com.webforj.component;

import com.webforj.annotation.AnnotationProcessor;
import com.webforj.component.ComponentLifecycleObserver.LifecycleEvent;
import com.webforj.concern.HasComponents;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;

/**
 * A class representing a registry for managing child components within a container component.
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
public final class ComponentRegistry implements HasComponents {
  private final Map<String, Component> components = new LinkedHashMap<>();
  private final Component parent;
  private final Consumer<Component> addConsumer;
  private final boolean allowMultipleAttach;
  private final ComponentLifecycleObserver observer =
      (Component component, LifecycleEvent event) -> {
        if (event == LifecycleEvent.DESTROY) {
          components.remove(component.getComponentId());
        }

        if (event == LifecycleEvent.CREATE) {
          AnnotationProcessor processor = new AnnotationProcessor();
          processor.processControlAnnotations(component);
        }
      };

  /**
   * Construct the registry for the given component.
   *
   * @param parent the component to construct the registry for.
   * @param consumer the consumer to be called when a component is added to the registry.
   * @param allowMultipleAttach whether to allow attaching the component to different containers.
   */
  public ComponentRegistry(Component parent, Consumer<Component> consumer,
      boolean allowMultipleAttach) {
    this.parent = parent;
    this.addConsumer = consumer;
    this.allowMultipleAttach = allowMultipleAttach;
  }

  /**
   * Construct the registry for the given component.
   *
   * @param parent the component to construct the registry for.
   * @param consumer the consumer to be called when a component is added to the registry.
   */
  public ComponentRegistry(Component parent, Consumer<Component> consumer) {
    this(parent, consumer, false);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void add(Component... components) {
    if (components == null) {
      throw new NullPointerException("Cannot add a null component");
    }

    for (Component current : components) {
      if (current == null) {
        throw new NullPointerException("Cannot add a null component");
      }

      // adding a component that is destroyed should throw an exception
      if (current.isDestroyed()) {
        throw new IllegalStateException("Cannot add a component that is destroyed");
      }

      // adding a component with the same id should throw an exception
      if (this.components.containsKey(current.getComponentId())) {
        throw new IllegalArgumentException(
            "Component with id '" + current.getClass().getName() + "' already exists");
      }

      // adding an already attached component should throw an exception
      if (current.isAttached() && !allowMultipleAttach) {
        throw new IllegalArgumentException("Component with id '" + current.getClass().getName()
            + "' is already attached to a different component");
      }

      // monitoring component lifecycle destruction
      current.addLifecycleObserver(observer);

      if (!parent.isAttached()) {
        this.components.put(current.getComponentId(), current);
      } else {
        addConsumer.accept(current);
      }
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void remove(Component... components) {
    if (components == null) {
      throw new NullPointerException("Cannot remove a null component");
    }

    for (Component c : components) {
      if (c == null) {
        throw new NullPointerException("Cannot remove a null component");
      }

      if (this.components.containsKey(c.getComponentId())) {
        this.components.remove(c.getComponentId());
        c.destroy();
      }
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void removeAll() {
    getComponents().forEach(Component::destroy);
    components.clear();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public List<Component> getComponents() {
    return Collections.unmodifiableList(new ArrayList<>(components.values()));
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Component getComponent(String id) {
    return components.get(id);
  }
}

