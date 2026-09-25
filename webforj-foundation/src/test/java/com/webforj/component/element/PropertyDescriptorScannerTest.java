package com.webforj.component.element;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

class PropertyDescriptorScannerTest {

  @Test
  void shouldFindAccessorsForDescriptorWithNullDefault() throws Exception {
    NullableProperties instance = new NullableProperties();
    var properties = PropertyDescriptorScanner.scan(NullableProperties.class, instance,
        descriptor -> true);

    assertEquals(2, properties.size());
    assertEquals(NullableProperties.class.getMethod("getText"), properties.get(0).getGetter());
    assertEquals(NullableProperties.class.getMethod("setText", String.class),
        properties.get(0).getSetter());
    assertEquals(NullableProperties.class.getMethod("isEnabled"), properties.get(1).getGetter());

    PropertyDescriptorTester.run(NullableProperties.class, instance);
    assertNull(instance.getText());
    assertNull(instance.isEnabled());
  }

  static class NullableProperties {
    final PropertyDescriptor<String> textProp = PropertyDescriptor.property("text", null);
    final PropertyDescriptor<Boolean> enabledProp = PropertyDescriptor.property("enabled", null);

    private String text = "initial";
    private Boolean enabled = true;

    public String getText() {
      return text;
    }

    public void setText(String text) {
      this.text = text;
    }

    public void setText(Integer value) {
      throw new AssertionError("The getter must select the String overload");
    }

    public Boolean isEnabled() {
      return enabled;
    }

    public void setEnabled(Boolean enabled) {
      this.enabled = enabled;
    }
  }
}
