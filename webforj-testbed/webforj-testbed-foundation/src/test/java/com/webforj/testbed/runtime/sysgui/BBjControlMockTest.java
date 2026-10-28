package com.webforj.testbed.runtime.sysgui;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.basis.bbj.proxies.event.BBjButtonPushEvent;
import com.basis.bbj.proxies.event.BBjGainedFocusEvent;
import com.basis.bbj.proxies.event.BBjLostFocusEvent;
import com.basis.bbj.proxies.sysgui.BBjButton;
import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import com.basis.startup.type.BBjException;
import com.google.gson.reflect.TypeToken;
import com.webforj.testbed.runtime.RuntimeMock;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.junit.jupiter.api.Test;

class BBjControlMockTest {

  @Test
  void shouldStartVisibleAndEnabled() throws BBjException {
    BBjControl control = new BBjControlMock(1).getProxy(BBjControl.class);

    assertTrue(control.isVisible());
    assertTrue(control.isEnabled());
  }

  @Test
  void shouldRoundTripPropertiesAttributesAndStyles() throws BBjException {
    BBjControlMock mock = new BBjControlMock(1);
    BBjControl control = mock.getProxy(BBjControl.class);

    control.setProperty("value", 42);
    control.setAttribute("id", "save");
    control.setStyle("color", "red");
    control.putClientProperty("key", "stored");

    assertEquals(42.0, control.getProperty("value"));
    assertEquals("save", control.getAttribute("id"));
    assertEquals("red", control.getStyle("color"));
    assertEquals("red", control.getComputedStyle("color"));
    assertEquals("stored", control.getClientProperty("key"));
    assertEquals(Map.of("value", "42"), mock.getProperties());
    assertEquals(Map.of("id", "save"), mock.getAttributes());
    assertTrue(control.hasAttribute("id"));

    control.removeAttribute("id");
    control.unsetStyle("color");

    assertFalse(control.hasAttribute("id"));
    assertNull(control.getAttribute("id"));
    assertNull(control.getStyle("color"));
  }

  @Test
  void shouldAnswerAnUnsetPropertyWithNullWhateverTheRequestedType() throws BBjException {
    BBjControl control = new BBjControlMock(1).getProxy(BBjControl.class);

    assertNull(control.getProperty("opened", Boolean.class));
    assertNull(control.getProperty("count", Integer.class));
    assertNull(control.getProperty("position", Double.class));
    assertNull(control.getProperty("total", Long.class));
    assertNull(control.getProperty("ratio", Float.class));
    assertNull(control.getProperty("small", Short.class));
    assertNull(control.getProperty("selected", new TypeToken<Set<String>>() {}.getType()));
    assertNull(control.getProperty("items", new TypeToken<List<String>>() {}.getType()));
    assertNull(control.getProperty("data", new TypeToken<Map<String, String>>() {}.getType()));
    assertNull(control.getProperty("label", String.class));
    assertNull(control.getProperty("label"));
  }

  @Test
  void shouldTrackStyleNamesAndClasses() throws BBjException {
    BBjControl control = new BBjControlMock(1).getProxy(BBjControl.class);

    control.addStyle("primary");
    control.addClass("highlight");

    assertEquals(List.of("primary"), control.getStyles());
    assertEquals(List.of("highlight"), control.getClassList());

    assertFalse(control.toggleClass("highlight"));
    assertTrue(control.toggleClass("selected"));

    control.removeStyle("primary");
    control.removeClass("selected");
    control.clearStyles();

    assertTrue(control.getStyles().isEmpty());
    assertTrue(control.getClassList().isEmpty());
  }

  @Test
  void shouldAddAndRemoveSpaceSeparatedClasses() throws BBjException {
    BBjControl control = new BBjControlMock(1).getProxy(BBjControl.class);

    control.addClass("primary selected");

    assertEquals(List.of("primary", "selected"), control.getClassList());
    control.addClass("highlight");
    control.removeClass("primary selected");
    assertEquals(List.of("highlight"), control.getClassList());
    assertEquals("highlight", control.getAttribute("class"));
    control.addClass("custom-theme chat-toggle-btn");
    assertEquals(List.of("highlight", "custom-theme", "chat-toggle-btn"), control.getClassList());
  }

  @Test
  void shouldValidateEveryClassBeforeChangingTheList() throws BBjException {
    BBjControl control = new BBjControlMock(1).getProxy(BBjControl.class);
    control.addClass("primary");

    assertThrows(BBjException.class, () -> control.addClass("selected invalid!"));
    assertEquals(List.of("primary"), control.getClassList());
    assertThrows(BBjException.class, () -> control.removeClass("primary invalid!"));
    assertEquals(List.of("primary"), control.getClassList());
    assertThrows(BBjException.class, () -> control.toggleClass("primary selected"));
    assertEquals(List.of("primary"), control.getClassList());
  }

  @Test
  void shouldKeepClassAttributeAndClassListInSync() throws BBjException {
    BBjControl control = new BBjControlMock(1).getProxy(BBjControl.class);

    control.setAttribute("class", " primary  selected primary ");

    assertEquals(List.of("primary", "selected"), control.getClassList());

    control.addClass("highlight");
    assertEquals("primary selected highlight", control.getAttribute("class"));

    control.removeClass("selected");
    assertEquals("primary highlight", control.getAttribute("class"));

    assertFalse(control.toggleClass("primary"));
    assertEquals("highlight", control.getAttribute("class"));

    control.setAttribute("class", "replacement");
    assertEquals(List.of("replacement"), control.getClassList());

    control.removeClass("replacement");
    assertEquals("", control.getAttribute("class"));
    assertTrue(control.getClassList().isEmpty());

    control.addClass("last");
    control.removeAttribute("class");
    assertFalse(control.hasAttribute("class"));
    assertTrue(control.getClassList().isEmpty());
  }

  @Test
  void shouldLeaveTheControlWithoutClassAttributeWhileTheClassListStaysEmpty() throws BBjException {
    BBjControl control = new BBjControlMock(1).getProxy(BBjControl.class);

    control.removeClass("absent");
    control.addClass(" ");

    assertFalse(control.hasAttribute("class"));
    assertNull(control.getAttribute("class"));
    assertTrue(control.toggleClass("primary"));
    assertFalse(control.toggleClass("primary"));
    assertEquals("", control.getAttribute("class"));
  }

  @Test
  void shouldRefuseClassNamesSeparatedByMoreThanOneSpace() throws BBjException {
    BBjControl control = new BBjControlMock(1).getProxy(BBjControl.class);

    BBjException failure =
        assertThrows(BBjException.class, () -> control.addClass("primary  selected"));

    assertEquals(BBjException.ARG_ERR, failure.getErrorNumber());
    assertTrue(control.getClassList().isEmpty());
  }

  @Test
  void shouldTrackDestruction() throws BBjException {
    BBjControlMock mock = new BBjControlMock(1);
    BBjControl control = mock.getProxy(BBjControl.class);

    assertFalse(control.isDestroyed());

    control.destroy();

    assertTrue(control.isDestroyed());
    assertTrue(mock.isDestroyed());
  }

  @Test
  void shouldExposeIdentifiers() throws BBjException {
    BBjControlMock mock = new BBjControlMock(7);
    BBjControl control = mock.getProxy(BBjControl.class);

    assertEquals(7, control.getID());
    assertEquals(7, mock.getId());
    assertFalse(control.getClientObjectID().isEmpty());
  }

  @Test
  void shouldStoreAndFireControlCallbacks() throws BBjException {
    BBjControlMock mock = new BBjControlMock(1);
    BBjControl control = mock.getProxy(BBjControl.class);

    List<Double> received = new ArrayList<>();
    Object handler = new Object() {
      @SuppressWarnings("unused")
      public void handleEvent(BBjButtonPushEvent event) {
        received.add(event.getX());
      }
    };

    control.setCallback(100, handler, "handleEvent");

    assertTrue(mock.hasCallback(100));
    assertTrue(
        mock.fireEvent(100, RuntimeMock.createEvent(BBjButtonPushEvent.class, Map.of("getX", 3d))));
    assertEquals(List.of(3d), received);

    control.clearCallback(100);

    assertFalse(mock.hasCallback(100));
    assertFalse(
        mock.fireEvent(100, RuntimeMock.createEvent(BBjButtonPushEvent.class, Map.of("getX", 3d))));
  }

  @Test
  void shouldReportTheFocusTheControlGainsAndTheFocusAnotherLoses() throws BBjException {
    BBjWindowMock windowMock = new BBjWindowMock(1);
    BBjWindow window = windowMock.getProxy(BBjWindow.class);
    BBjButton first = window.addButton(100, "First");
    BBjButton second = window.addButton(101, "Second");
    List<String> reported = new ArrayList<>();
    Object handler = new Object() {
      @SuppressWarnings("unused")
      public void gained(BBjGainedFocusEvent event) {
        reported.add("gained " + event.getText() + " " + event.isTemporary());
      }

      @SuppressWarnings("unused")
      public void lost(BBjLostFocusEvent event) {
        reported.add("lost " + event.getText());
      }
    };
    for (BBjButton button : List.of(first, second)) {
      button.setCallback(SysGuiEventConstants.ON_GAINED_FOCUS, handler, "gained");
      button.setCallback(SysGuiEventConstants.ON_LOST_FOCUS, handler, "lost");
    }

    first.focus();
    second.focus();
    second.focus();

    assertEquals(List.of("gained First false", "lost First", "gained Second false"), reported);
    assertEquals(true, second.getProperty("hasFocus", Boolean.class));
  }

  @Test
  void shouldExposePublicState() {
    BBjControlMock mock = new BBjControlMock(1);

    mock.putState("selected", true);

    assertEquals(true, mock.getState("selected"));
  }

  @Test
  void shouldResolveBackingMockFromProxy() {
    BBjControlMock mock = new BBjControlMock(1);
    BBjControl control = mock.getProxy(BBjControl.class);

    assertEquals(mock, BBjWindowMock.mockOf(control));
  }

  @Test
  void shouldRefuseFocusWhileTheControlOrTheWindowAboveItIsDisabled() throws BBjException {
    BBjWindowMock windowMock = new BBjWindowMock(1);
    BBjWindow window = windowMock.getProxy(BBjWindow.class);
    BBjButton first = window.addButton(100, "First");
    BBjButton second = window.addButton(101, "Second");

    first.focus();

    assertEquals(true, first.getProperty("hasFocus", Boolean.class));

    second.setEnabled(false);
    second.focus();

    assertEquals(true, first.getProperty("hasFocus", Boolean.class));
    assertEquals(false, second.getProperty("hasFocus", Boolean.class));

    second.setEnabled(true);
    window.setEnabled(false);
    second.focus();

    assertEquals(false, second.getProperty("hasFocus", Boolean.class));

    window.setEnabled(true);
    second.focus();

    assertEquals(false, first.getProperty("hasFocus", Boolean.class));
    assertEquals(true, second.getProperty("hasFocus", Boolean.class));
  }
}
