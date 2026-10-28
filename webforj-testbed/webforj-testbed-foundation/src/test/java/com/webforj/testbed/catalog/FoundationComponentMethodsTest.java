package com.webforj.testbed.catalog;

import com.webforj.component.Component;
import com.webforj.component.element.Element;
import com.webforj.component.window.Frame;
import com.webforj.testbed.TestbedTestSupport;
import com.webforj.testbed.WebforjTestbed;
import java.lang.reflect.InvocationTargetException;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

class FoundationComponentMethodsTest extends TestbedTestSupport {

  @ParameterizedTest(name = "{0}")
  @MethodSource("com.webforj.testbed.catalog.FoundationComponents#discover")
  void shouldCarryEveryPublicMethod(Class<? extends Component> type) {
    WebforjTestbed context = WebforjTestbed.getCurrent();
    Component component = attach(type, context);
    ComponentMethodVerifier.verifyEveryPublicMethod(component);
  }

  private static Component attach(Class<? extends Component> type, WebforjTestbed context) {
    if (type == Frame.class) {
      return context.getFrame();
    }

    Component component = type == Element.class ? new Element("div") : newInstance(type);
    context.add(component);

    return component;
  }

  private static Component newInstance(Class<? extends Component> type) {
    try {
      return type.getDeclaredConstructor().newInstance();
    } catch (InstantiationException | IllegalAccessException | NoSuchMethodException
        | InvocationTargetException e) {
      throw new IllegalStateException(type.getName() + " has no usable default constructor", e);
    }
  }
}
