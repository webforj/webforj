package org.dwcj.component.html.elements;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.util.List;
import org.dwcj.component.element.PropertyDescriptorTester;
import org.junit.jupiter.api.Test;

class IframeTest {

  @Test
  void shouldSetGetProperties() {
    Iframe component = new Iframe();

    try {
      PropertyDescriptorTester.run(Iframe.class, component);
    } catch (Exception e) {
      fail("PropertyDescriptor test failed: " + e.getMessage());
    }
  }

  @Test
  void testSetAndGetSandboxValues() {
    Iframe component = new Iframe();

    Iframe.Sandbox sandbox1 = Iframe.Sandbox.ALLOW_SCRIPTS;
    Iframe.Sandbox sandbox2 = Iframe.Sandbox.ALLOW_FORMS;

    component.addSandbox(sandbox1);
    component.addSandbox(sandbox2);

    assertTrue(component.getSandboxValues().contains(sandbox1));
    assertTrue(component.getSandboxValues().contains(sandbox2));
    assertEquals(component.getElement().getAttribute("sandbox"), "allow-scripts allow-forms");
  }

  @Test
  void testRemoveSandboxValue() {
    Iframe component = new Iframe();

    Iframe.Sandbox sandbox1 = Iframe.Sandbox.ALLOW_SCRIPTS;
    Iframe.Sandbox sandbox2 = Iframe.Sandbox.ALLOW_FORMS;

    component.addSandbox(sandbox1);
    component.addSandbox(sandbox2);

    assertTrue(component.getSandboxValues().contains(sandbox1));
    assertTrue(component.getSandboxValues().contains(sandbox2));

    component.removeSandboxValue(sandbox1);

    assertFalse(component.getSandboxValues().contains(sandbox1));
    assertTrue(component.getSandboxValues().contains(sandbox2));
    assertEquals(component.getElement().getAttribute("sandbox"), "allow-forms");
  }

  @Test
  void testSetSandbox() {
    Iframe component = new Iframe();

    Iframe.Sandbox sandbox1 = Iframe.Sandbox.ALLOW_SCRIPTS;
    Iframe.Sandbox sandbox2 = Iframe.Sandbox.ALLOW_FORMS;
    Iframe.Sandbox sandbox3 = Iframe.Sandbox.ALLOW_POPUPS;

    component.setSandbox(List.of(sandbox1, sandbox2, sandbox3));

    assertTrue(component.getSandboxValues().contains(sandbox1));
    assertTrue(component.getSandboxValues().contains(sandbox2));
    assertTrue(component.getSandboxValues().contains(sandbox3));
  }
}
