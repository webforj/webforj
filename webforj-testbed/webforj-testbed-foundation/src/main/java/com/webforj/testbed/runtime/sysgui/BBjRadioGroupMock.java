package com.webforj.testbed.runtime.sysgui;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.sysgui.BBjRadioButton;
import com.basis.startup.type.BBjException;
import com.webforj.testbed.runtime.RuntimeMock;
import com.webforj.testbed.runtime.event.BBjControlEventCallbackRegistry;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

/**
 * The mock backing the radio group.
 *
 * <p>
 * Driven by {@code com.webforj.component.optioninput.RadioButtonGroup}, which adds and removes
 * member buttons, names the group, reads the selected member back expecting null when nothing is
 * selected, and registers the selection change callback.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public class BBjRadioGroupMock extends RuntimeMock {
  private final List<BBjRadioButton> members = new ArrayList<>();
  private final BBjControlEventCallbackRegistry callbacks = new BBjControlEventCallbackRegistry();
  private BBjRadioButton selected = null;

  /**
   * Returns the member buttons of the group, in registration order.
   *
   * @return the member buttons
   */
  public List<BBjRadioButton> getMembers() {
    return List.copyOf(members);
  }

  /**
   * Selects the given member button.
   *
   * @param member the member to select
   */
  public void select(BBjRadioButton member) {
    selected = member;
  }

  /**
   * Returns the selected member, null when none is selected.
   *
   * @return the selected member
   */
  public BBjRadioButton getSelectedMember() {
    return selected;
  }

  void deselect(BBjRadioButton member) {
    if (selected == member) {
      selected = null;
    }
  }

  /**
   * Makes the given member the selected one and deselects the member selected before, without
   * firing a change, the way selecting a button through the API does.
   *
   * @param member the member to select
   * @throws BBjException if the previous member rejects the deselection
   */
  public void moveSelectionTo(BBjRadioButton member) throws BBjException {
    BBjRadioButton previous = selected;
    if (previous == member) {
      return;
    }

    selected = member;
    if (previous != null) {
      previous.setSelected(false);
    }
  }

  /**
   * Fires a group event through the callback registered for the given event type.
   *
   * @param eventType the event type constant the callback was registered under
   * @param event the event to deliver
   *
   * @return true when a callback was registered and invoked, false otherwise
   */
  public boolean fireEvent(int eventType, BBjEvent event) {
    return callbacks.fire(eventType, event);
  }

  public BBjControlEventCallbackRegistry getCallbacks() {
    return callbacks;
  }

  @Override
  protected Object handle(Object proxy, Method method, Object[] args) throws Exception {
    return switch (method.getName()) {
      case "add" -> {
        BBjRadioButton member = (BBjRadioButton) args[0];
        if (members.contains(member)) {
          throw new BBjException("The radio button is already a member of the group",
              BBjException.ARG_ERR);
        }
        members.add(member);
        // A button that joins already selected becomes the group's selection.
        if (member.isSelected()) {
          moveSelectionTo(member);
        }
        yield null;
      }
      case "remove" -> {
        if (!members.remove(args[0])) {
          throw new BBjException("The radio button is not a member of the group",
              BBjException.ARG_ERR);
        }
        if (selected == args[0]) {
          selected = null;
        }
        yield null;
      }
      case "getSelected" -> selected;
      case "setCallback" -> {
        if (args.length == 3 && args[0] instanceof Integer eventType
            && args[2] instanceof String methodName) {
          callbacks.register(eventType, args[1], methodName);
        }
        yield null;
      }
      case "clearCallback" -> {
        if (args.length == 1 && args[0] instanceof Integer eventType) {
          callbacks.clear(eventType);
        }
        yield null;
      }
      default -> UNHANDLED;
    };
  }
}
