package com.webforj.testbed.runtime.sysgui;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.basis.bbj.proxies.sysgui.BBjButton;
import com.basis.bbj.proxies.sysgui.BBjCEdit;
import com.basis.bbj.proxies.sysgui.BBjCheckBox;
import com.basis.bbj.proxies.sysgui.BBjEditBox;
import com.basis.bbj.proxies.sysgui.BBjListBox;
import com.basis.bbj.proxies.sysgui.BBjTabCtrl;
import com.basis.bbj.proxies.sysgui.BBjTree;
import com.basis.bbj.proxies.sysgui.BBjWebComponent;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.junit.jupiter.api.Test;

class BBjWindowMockTest {

  @Test
  void shouldCreateChildControlsThroughFactories() throws BBjException {
    BBjWindowMock window = new BBjWindowMock(1);
    BBjWindow proxy = window.getProxy(BBjWindow.class);

    BBjButton button = proxy.addButton(proxy.getAvailableControlID(), "Save");

    assertNotNull(button);
    assertEquals("Save", button.getText());
    assertEquals(1, window.getChildren().size());
    assertEquals(window, BBjWindowMock.mockOf(button).getParent().orElseThrow());
    assertEquals(window, BBjWindowMock.mockOf(button.getParentWindow()));
  }

  @Test
  void shouldCreateTheDedicatedMockPerFactoryMethod() throws BBjException {
    BBjWindowMock window = new BBjWindowMock(1);
    BBjWindow proxy = window.getProxy(BBjWindow.class);

    BBjButton button = proxy.addButton(1, "Save");
    assertInstanceOf(BBjButtonMock.class, BBjWindowMock.mockOf(button));
    BBjCheckBox checkBox = proxy.addCheckBox(2, "");
    assertInstanceOf(BBjCheckBoxMock.class, BBjWindowMock.mockOf(checkBox));
    BBjEditBox editBox = proxy.addEditBox(3, "");
    assertInstanceOf(BBjEditBoxMock.class, BBjWindowMock.mockOf(editBox));
    BBjCEdit textArea = proxy.addCEdit(4, "");
    assertInstanceOf(BBjCEditMock.class, BBjWindowMock.mockOf(textArea));
    BBjListBox listBox = proxy.addListBox(5, "");
    assertInstanceOf(BBjListBoxMock.class, BBjWindowMock.mockOf(listBox));
    BBjTree tree = proxy.addTree(6);
    assertInstanceOf(BBjTreeMock.class, BBjWindowMock.mockOf(tree));
    BBjTabCtrl tabCtrl = proxy.addTabCtrl(7);
    assertInstanceOf(BBjTabCtrlMock.class, BBjWindowMock.mockOf(tabCtrl));
    BBjWebComponent webComponent = proxy.addWebComponent("x-chip");
    assertInstanceOf(BBjWebComponentMock.class, BBjWindowMock.mockOf(webComponent));
    BBjWindow child = proxy.addChildWindow(8, "", 0);
    assertInstanceOf(BBjWindowMock.class, BBjWindowMock.mockOf(child));
    assertNotNull(child.addButton(0, "Nested"));
  }

  @Test
  void shouldStartEditableControlsEditable() throws BBjException {
    BBjWindowMock window = new BBjWindowMock(1);
    BBjWindow proxy = window.getProxy(BBjWindow.class);

    BBjEditBox editBox = proxy.addEditBox(proxy.getAvailableControlID(), "");

    assertTrue(editBox.isEditable());
  }

  @Test
  void shouldAnswerTextSelectionWithFourEntries() throws BBjException {
    BBjWindowMock window = new BBjWindowMock(1);
    BBjWindow proxy = window.getProxy(BBjWindow.class);

    BBjCEdit textArea = proxy.addCEdit(proxy.getAvailableControlID(), "");
    BBjEditBox editBox = proxy.addEditBox(proxy.getAvailableControlID(), "");

    assertEquals(4, textArea.getSelection().size());
    assertEquals(4, editBox.getSelection().size());
  }

  @Test
  void shouldHandOutTheLowestFreeControlIdentifier() throws BBjException {
    BBjWindowMock window = new BBjWindowMock(1);
    BBjWindow proxy = window.getProxy(BBjWindow.class);

    int first = proxy.getAvailableControlID();

    assertEquals(first, proxy.getAvailableControlID());

    BBjButton button = proxy.addButton(first, "First");
    int second = proxy.getAvailableControlID();

    assertTrue(second > first);

    button.destroy();

    assertEquals(first, proxy.getAvailableControlID());
  }

  @Test
  void shouldRefuseDuplicateControlIdentifiers() throws BBjException {
    BBjWindowMock window = new BBjWindowMock(1);
    BBjWindow proxy = window.getProxy(BBjWindow.class);
    int id = proxy.getAvailableControlID();
    proxy.addButton(id, "First");

    assertThrows(BBjException.class, () -> proxy.addButton(id, "Second"));
  }
}
