package com.webforj.devtools.craftforj.inspector.action;

import com.google.gson.JsonObject;
import com.webforj.component.Component;
import com.webforj.devtools.craftforj.action.CraftforjActionException;
import com.webforj.devtools.craftforj.action.CraftforjActionHandler;
import com.webforj.devtools.craftforj.utilities.ComponentLocator;
import java.util.Optional;
import java.util.function.Function;

/**
 * Action handler that destroys a component.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class DestroyComponentAction implements CraftforjActionHandler<Void> {

  /** The action name. */
  public static final String ACTION = "inspector.destroyComponent";

  private final Function<String, Optional<Component>> componentFinder;

  /**
   * Creates a new DestroyComponentAction with the default component locator.
   */
  public DestroyComponentAction() {
    this(ComponentLocator::findById);
  }

  /**
   * Creates a new DestroyComponentAction with a custom component finder.
   *
   * @param componentFinder function to find components by ID
   */
  public DestroyComponentAction(Function<String, Optional<Component>> componentFinder) {
    this.componentFinder = componentFinder;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getAction() {
    return ACTION;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Void handle(JsonObject params) {
    String componentId = params.has("componentId") ? params.get("componentId").getAsString() : null;

    if (componentId == null || componentId.isEmpty()) {
      throw new CraftforjActionException("Missing componentId parameter");
    }

    Optional<Component> component = componentFinder.apply(componentId);
    if (component.isEmpty()) {
      throw new CraftforjActionException("Component not found: " + componentId);
    }

    component.get().destroy();

    return null;
  }
}
