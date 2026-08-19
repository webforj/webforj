package com.webforj.component.layout.appnav;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.component.Component;
import com.webforj.component.element.Element;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class AppNavLabelTest {

  AppNavLabel component;

  @BeforeEach
  void setUp() {
    component = new AppNavLabel();
  }

  @Nested
  class ConstructorsTest {

    @Test
    void shouldCreateLabelWithText() {
      String text = "Management";

      AppNavLabel label = new AppNavLabel(text);

      assertEquals(text, label.getText());
    }

    @Test
    void shouldCreateLabelWithTextAndPrefix() {
      String text = "Management";
      Component prefix = new Element("div");

      AppNavLabel label = new AppNavLabel(text, prefix);

      assertEquals(text, label.getText());
      assertEquals(prefix, label.getPrefixComponent());
    }
  }

  @Nested
  class PrefixSuffixApi {

    @Test
    void shouldSetPrefixAndSuffix() {
      Element prefix = new Element("span");
      Element suffix = new Element("span");
      component.setPrefixComponent(prefix);
      component.setSuffixComponent(suffix);

      assertSame(prefix, component.getPrefixComponent());
      assertSame(suffix, component.getSuffixComponent());
    }

    @Test
    void shouldDestroyPreviousPrefixAndSuffix() {
      Element oldPrefix = new Element("span");
      Element oldSuffix = new Element("span");
      component.setPrefixComponent(oldPrefix);
      component.setSuffixComponent(oldSuffix);

      Element newPrefix = new Element("span");
      Element newSuffix = new Element("span");
      component.setPrefixComponent(newPrefix);
      component.setSuffixComponent(newSuffix);

      assertTrue(oldPrefix.isDestroyed());
      assertTrue(oldSuffix.isDestroyed());

      assertSame(newPrefix, component.getPrefixComponent());
      assertSame(newSuffix, component.getSuffixComponent());
    }

    @Test
    void shouldIgnoreSamePrefixAndSuffix() {
      Element prefix = new Element("span");
      Element suffix = new Element("span");
      component.setPrefixComponent(prefix);
      component.setSuffixComponent(suffix);

      component.setPrefixComponent(prefix);
      component.setSuffixComponent(suffix);

      assertFalse(prefix.isDestroyed());
      assertFalse(suffix.isDestroyed());

      assertSame(prefix, component.getPrefixComponent());
      assertSame(suffix, component.getSuffixComponent());
    }
  }
}
