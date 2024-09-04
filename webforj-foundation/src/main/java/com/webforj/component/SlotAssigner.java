package com.webforj.component;

import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.startup.type.BBjException;
import com.webforj.bridge.ComponentAccessor;
import com.webforj.exceptions.WebforjRuntimeException;
import java.util.Objects;
import java.util.Optional;

/**
 * {@code SlotAssigner} is responsible for binding multiple components to a single target component.
 * The components are assigned to slots in the target component's window.
 *
 * <p>
 * Once a slot is assigned to a component, the associated control from the slot is linked with the
 * control of the target component in the specified slot.
 * </p>
 *
 * <p>
 * This process allows for dynamic and flexible component slot management, making it possible to
 * assign multiple slots to a single component and manage their lifecycle.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public final class SlotAssigner {
  /**
   * A functional interface for assigning slot controls to the target control.
   *
   * @see SlotAssigner#assign(SlotAssigner.Assigner)
   *
   * @author Hyyan Abo Fakher
   * @since 24.11
   */
  @FunctionalInterface
  public interface Assigner {
    /**
     * Assigns the slot control to the target control.
     *
     * @param slot the slot name to which the slot control is assigned.
     * @param targetControl the target control to which the slot control is assigned.
     * @param slotControl the slot control to be assigned to the target control.
     */
    void assign(String slot, BBjControl targetControl, BBjControl slotControl);
  }

  private final Component targetComponent;
  private final SlotAssigner.Assigner assigner;
  private final SlotRegistry slotRegistry;
  private boolean attached = false;

  /**
   * Constructs a new {@code SlotAssigner} to bind multiple components to the given target
   * component.
   *
   * @param targetComponent the target component to which slots will be assigned.
   * @param assigner an optional assigner to handle the slot assignment logic.
   * @param slotRegistry the slot registry to manage slots.
   *
   * @throws NullPointerException if {@code targetComponent} is null.
   */
  public SlotAssigner(Component targetComponent, SlotAssigner.Assigner assigner,
      SlotRegistry slotRegistry) {
    Objects.requireNonNull(targetComponent, "SlotAssigner target component must not be null");
    Objects.requireNonNull(slotRegistry, "SlotRegistry must not be null");

    this.targetComponent = targetComponent;
    this.assigner = assigner;
    this.slotRegistry = slotRegistry;
  }


  /**
   * Constructs a new {@code SlotAssigner} to bind multiple components to the given target
   * component.
   *
   * @param targetComponent the target component to which slots will be assigned.
   * @param assigner an optional assigner to handle the slot assignment logic.
   *
   * @throws NullPointerException if {@code targetComponent} is null.
   */
  public SlotAssigner(Component targetComponent, SlotAssigner.Assigner assigner) {
    this(targetComponent, assigner, new SlotRegistry());
  }

  /**
   * Constructs a new {@code SlotAssigner} to bind multiple components to the given target
   * component.
   *
   * @param targetComponent the target component to which slots will be assigned.
   *
   * @throws NullPointerException if {@code targetComponent} is null.
   */
  public SlotAssigner(Component targetComponent) {
    this(targetComponent, null);
  }

  /**
   * Returns the slot registry associated with this slot assigner.
   *
   * @return the slot registry.
   */
  public SlotRegistry getSlotRegistry() {
    return slotRegistry;
  }

  /**
   * Returns the first component assigned to the given slot.
   *
   * @param slot the slot name to which the component is assigned.
   * @return the component assigned to the slot, or {@code null} if no component is assigned.
   */
  public Component getSlotComponent(String slot) {
    return slotRegistry.getFirstComponentInSlot(slot);
  }

  /**
   * Assigns the given component to the target component.
   *
   * @param slot the slot name to which the component is assigned.
   * @param component the component to be assigned.
   *
   * @throws IllegalStateException if the slots have already been assigned.
   * @throws WebforjRuntimeException if an error occurs during slot assignment.
   */
  public void assign(String slot, Component component) {
    assignComponentToSlot(slot, component, false);
  }

  /**
   * reAssigns the given component to the target component.
   *
   * @param slot the slot name to which the component is assigned.
   * @param component the component to be assigned.
   *
   * @throws IllegalStateException if the slots have already been assigned.
   * @throws WebforjRuntimeException if an error occurs during slot assignment.
   */
  public void reAssign(String slot, Component component) {
    assignComponentToSlot(slot, component, true);
  }

  /**
   * Catchup all queued slot assignments and link the slot controls to the target control.
   *
   * <p>
   * This method should be called after the target component is attached to the window. It assigns
   * the slot controls to the target control in the specified slot.
   * </p>
   *
   * @throws IllegalStateException if {@code attach()} has already been called.
   */
  public void attach() {
    if (isAttached()) {
      throw new IllegalStateException(
          "attach() has already been called. Slots cannot be attached multiple times.");
    }

    attached = true;
    slotRegistry.getSlots().forEach(slot -> {
      slotRegistry.getComponentsInSlot(slot).forEach(component -> {
        linkComponentToSlotControl(slot, component);
      });
    });
  }

  /**
   * Queues the slot assignment process to be executed.
   *
   * <p>
   * This method can be called before the target component is attached to the window. The slot
   * actual assignment will be executed when the target component is attached. If the target
   * component is already attached, the slot assignment will be executed immediately.
   * </p>
   *
   * @param slot the slot name to which the given component is assigned.
   * @param component the component to be assigned.
   * @param replace a flag indicating whether to replace the existing components in the slot.
   *        {@code true} to replace the existing components, {@code false} to add the component to
   *        the slot.
   *
   * @throws IllegalStateException if the slots have already been assigned or the target component
   *         is destroyed.
   * @throws WebforjRuntimeException if an error occurs during slot assignment.
   */
  private void assignComponentToSlot(String slot, Component component, boolean replace) {
    if (component.isDestroyed()) {
      throw new IllegalStateException("Cannot assign a destroyed component to slot \"" + slot
          + "\". Ensure the component is not destroyed before assignment.");
    }

    // Add the component to the slot registry
    if (replace) {
      slotRegistry.replaceComponentsInSlot(slot, component);
    } else {
      slotRegistry.addComponentsToSlot(slot, component);
    }

    if (isAttached()) {
      linkComponentToSlotControl(slot, component);
    }
  }

  /**
   * Links the given component's control to the target control in the specified slot.
   *
   * <p>
   * This method is called when the target component is attached to the window. It assigns the slot
   * controls to the target control in the specified slot.
   * </p>
   *
   * @param slot the slot name to which the component is assigned.
   * @param component the component to be assigned.
   *
   * @throws WebforjRuntimeException if an error occurs during slot assignment.
   */
  private void linkComponentToSlotControl(String slot, Component component) {
    if (component.isDestroyed()) {
      throw new IllegalStateException("Cannot link a destroyed component to slot \"" + slot
          + "\". Ensure the component is not destroyed before linking.");
    }

    // If the slot's component is not attached we need to attach it to the target component's window
    if (!component.isAttached()) {
      targetComponent.getWindow().add(component);
    }

    BBjControl targetControl = getControl(targetComponent);
    BBjControl slotControl = getControl(component);
    String theSlot = Objects.equals(slot, SlotRegistry.DEFAULT_SLOT) ? "" : slot;
    Optional.ofNullable(assigner)
        .ifPresentOrElse(invoker -> invoker.assign(theSlot, targetControl, slotControl), () -> {
          try {
            slotControl.setVisible(false);
            targetControl.setSlot(theSlot, slotControl);
            slotControl.setVisible(true);
          } catch (BBjException e) {
            String message = "Failed to assign slot \"" + slot + "\" to component \""
                + this.targetComponent.getClass().getName() + "\".";

            throw new WebforjRuntimeException(message, e);
          }
        });
  }

  BBjControl getControl(Component component) {
    try {
      return ComponentAccessor.getDefault().getControl(component);
    } catch (IllegalAccessException e) {
      throw new WebforjRuntimeException(
          "Failed to access control for component: " + component.getClass().getName(), e);
    }
  }

  boolean isAttached() {
    return attached;
  }
}
