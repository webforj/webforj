package com.webforj.testbed.runtime.sysgui;

import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.bbj.proxies.sysgui.BBjRadioButton;
import com.basis.bbj.proxies.sysgui.BBjRadioGroup;
import com.basis.bbj.proxies.sysgui.BBjTopLevelWindow;
import com.basis.startup.type.BBjException;
import com.webforj.bridge.ComponentAccessor;
import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;
import com.webforj.component.Composite;
import com.webforj.testbed.runtime.RuntimeMock;
import com.webforj.testbed.runtime.ScriptAnswerRegistry;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * The mock backing a window under test.
 *
 * <p>
 * Driven by {@code com.webforj.component.window.Window} and every component whose creation runs
 * through the window. The mock is the control factory of the window. Each factory method creates
 * the dedicated mock of the control family it produces, implementing exactly the requested
 * interface, so each webforJ component receives a control of the type and behavior its source
 * expects. The mapping between factory methods and mocks follows the component sources one to one.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public class BBjWindowMock extends BBjControlMock {
  static final Class<BBjTopLevelWindow> WINDOW_TYPE = BBjTopLevelWindow.class;

  private final AtomicInteger controlIds;
  private final ScriptAnswerRegistry scriptAnswers;
  private final List<BBjControlMock> children = new ArrayList<>();
  private final List<BBjRadioGroupMock> radioGroups = new ArrayList<>();

  /**
   * Creates a new window mock.
   *
   * @param id the window identifier
   */
  BBjWindowMock(int id) {
    this(id, new AtomicInteger(id));
  }

  BBjWindowMock(int id, AtomicInteger controlIds) {
    this(id, controlIds, new ScriptAnswerRegistry());
  }

  BBjWindowMock(int id, AtomicInteger controlIds, ScriptAnswerRegistry scriptAnswers) {
    super(id);
    this.controlIds = controlIds;
    this.scriptAnswers = scriptAnswers;
  }

  /**
   * Returns the control mocks created through this window, in creation order.
   *
   * @return the child control mocks
   */
  public List<BBjRadioGroupMock> getRadioGroups() {
    return List.copyOf(radioGroups);
  }

  public List<BBjControlMock> getChildren() {
    return List.copyOf(children);
  }

  /**
   * Resolves the runtime control of the given component, unwrapping composites.
   *
   * @param component the component
   * @return the control, or null when the component has none
   */
  public static BBjControl controlOf(Component component) {
    Component target = component;
    if (component instanceof Composite<?>) {
      target = ComponentUtil.getBoundComponent(component);
    }

    try {
      return ComponentAccessor.getDefault().getControl(target);
    } catch (IllegalAccessException e) {
      return null;
    }
  }

  /**
   * Finds the radio group the given control is a member of, on the window it was added to or one of
   * the enclosing windows.
   *
   * @param control the control mock
   *
   * @return the group, or an empty optional when the control is in no group
   */
  public static Optional<BBjRadioGroupMock> groupContaining(BBjControlMock control) {
    BBjWindowMock parent = control.getParent().orElse(null);
    while (parent != null) {
      for (BBjRadioGroupMock candidate : parent.getRadioGroups()) {
        for (BBjRadioButton member : candidate.getMembers()) {
          if (RuntimeMock.getBacking(member).orElse(null) == control) {
            return Optional.of(candidate);
          }
        }
      }

      parent = parent.getParent().orElse(null);
    }

    return Optional.empty();
  }

  /**
   * Returns the control mock backing the given control proxy.
   *
   * @param control the control proxy created by a runtime mock
   *
   * @return the backing control mock
   * @throws IllegalArgumentException if the control was not created by a runtime mock
   */
  public static BBjControlMock mockOf(BBjControl control) {
    if (RuntimeMock.getBacking(control).orElse(null) instanceof BBjControlMock controlMock) {
      return controlMock;
    }

    throw new IllegalArgumentException(
        "The control is not backed by a runtime mock: " + control.getClass().getName());
  }

  @Override
  protected Object handle(Object proxy, Method method, Object[] args) throws Exception {
    String name = method.getName();

    if (name.equals("getAvailableControlID")) {
      return availableControlId();
    }

    if (name.equals("addRadioGroup")) {
      BBjRadioGroupMock group = new BBjRadioGroupMock();
      radioGroups.add(group);

      return group.getProxy(BBjRadioGroup.class);
    }

    if (name.startsWith("add") && BBjControl.class.isAssignableFrom(method.getReturnType())) {
      return createChild(name, method.getReturnType(), args);
    }

    return super.handle(proxy, method, args);
  }

  private Object createChild(String factoryMethod, Class<?> controlType, Object[] args)
      throws BBjException {
    int childId = firstIntArgument(args);
    // The runtime refuses a second control with the id of a live one on the same window.
    for (BBjControlMock existing : children) {
      if (existing.getId() == childId) {
        throw new BBjException("Duplicate ID " + childId, BBjException.ARG_ERR);
      }
    }
    BBjControlMock child = createFamilyMock(factoryMethod, childId);

    // The first string a factory takes is the initial text of the control, except for a web
    // component, where it is the tag name and the element starts without text.
    String text = firstStringArgument(args);
    if (text != null && !(child instanceof BBjWebComponentMock)) {
      child.putState("text", text);
    }

    child.setParent(this);
    children.add(child);

    return child.getProxy(controlType);
  }

  /**
   * Creates the dedicated mock for the control family the factory method produces.
   *
   * <p>
   * The mapping mirrors the component sources. Every component's creation call is listed with its
   * dedicated mock, and factory methods no component calls fall back to the base control mock.
   * </p>
   */
  private BBjControlMock createFamilyMock(String factoryMethod, int childId) {
    return switch (factoryMethod) {
      case "addButton" -> new BBjButtonMock(childId);
      case "addCheckBox" -> new BBjCheckBoxMock(childId);
      case "addRadioButton" -> new BBjRadioButtonMock(childId);
      case "addStaticText" -> new BBjStaticTextMock(childId);
      case "addEditBox" -> new BBjEditBoxMock(childId);
      case "addEditBoxSpinner" -> new BBjEditBoxSpinnerMock(childId);
      case "addCEdit" -> new BBjCEditMock(childId);
      case "addInputE" -> new BBjInputEMock(childId);
      case "addInputESpinner" -> new BBjInputESpinnerMock(childId);
      case "addInputN" -> new BBjInputNMock(childId);
      case "addInputNSpinner" -> new BBjInputNSpinnerMock(childId);
      case "addInputD" -> new BBjInputDMock(childId);
      case "addInputDSpinner" -> new BBjInputDSpinnerMock(childId);
      case "addInputT" -> new BBjInputTMock(childId);
      case "addInputTSpinner" -> new BBjInputTSpinnerMock(childId);
      case "addListButton" -> new BBjListButtonMock(childId);
      case "addListEdit" -> new BBjListEditMock(childId);
      case "addListBox" -> new BBjListBoxMock(childId);
      case "addTree" -> new BBjTreeMock(childId);
      case "addTabCtrl" -> new BBjTabCtrlMock(childId);
      case "addNavigator" -> new BBjNavigatorMock(childId);
      case "addHorizontalSlider" -> new BBjSliderMock(childId);
      case "addProgressBar" -> new BBjProgressBarMock(childId);
      case "addFileChooser" -> new BBjFileChooserMock(childId);
      case "addWebComponent" -> new BBjWebComponentMock(childId, scriptAnswers);
      case "addChildWindow" -> new BBjWindowMock(childId, controlIds, scriptAnswers);
      case "addWindow" -> new BBjTopLevelWindowMock(childId, scriptAnswers);
      default -> new BBjControlMock(childId);
    };
  }

  private int firstIntArgument(Object[] args) {
    for (Object arg : args) {
      if (arg instanceof Integer intArg) {
        return intArg;
      }
    }

    return availableControlId();
  }

  // The runtime hands out the lowest id at or above 100 that no live child of this window holds.
  private int availableControlId() {
    int candidate = 100;
    while (holdsId(candidate)) {
      candidate++;
    }

    return candidate;
  }

  private boolean holdsId(int id) {
    for (BBjControlMock child : children) {
      if (child.getId() == id) {
        return true;
      }
    }

    return false;
  }

  void removeChild(BBjControlMock child) {
    children.remove(child);
  }

  private String firstStringArgument(Object[] args) {
    for (Object arg : args) {
      if (arg instanceof String stringArg && !stringArg.isEmpty()) {
        return stringArg;
      }
    }

    return null;
  }
}
