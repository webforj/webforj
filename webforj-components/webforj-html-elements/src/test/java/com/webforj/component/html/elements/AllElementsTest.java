package com.webforj.component.html.elements;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.fail;

import com.webforj.component.Component;
import com.webforj.component.html.HtmlComponent;
import com.webforj.component.html.HtmlComponentContainer;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import org.junit.jupiter.api.Test;

class AllElementsTest {

  @Test
  void testAllElementsWithConstructors()
      throws NoSuchMethodException, SecurityException, InstantiationException,
      IllegalAccessException, IllegalArgumentException, InvocationTargetException {

    Class<?>[] elementClasses = {Anchor.class, Article.class, Aside.class, Div.class,
        Emphasis.class, Footer.class, H1.class, H2.class, H3.class, H4.class, H5.class, H6.class,
        Header.class, Break.class, Legend.class, ListEntry.class, Main.class, Nav.class,
        OrderedList.class, Paragraph.class, FormattedText.class, Section.class, Span.class,
        Strong.class, UnorderedList.class};

    for (Class<?> elementClass : elementClasses) {
      // Test the default constructor
      Constructor<?> defaultConstructor = elementClass.getConstructor();
      HtmlComponent<?> defaultComponent = (HtmlComponent<?>) defaultConstructor.newInstance();
      assertNotNull(defaultComponent.getElement().getNodeName());

      // Tests for containers only
      if (defaultComponent instanceof HtmlComponentContainer<?>) {
        // Test the constructor with text
        Constructor<?> textConstructor = elementClass.getConstructor(String.class);
        String text = "text";
        HtmlComponent<?> textComponent = (HtmlComponent<?>) textConstructor.newInstance(text);
        assertEquals(text, textComponent.getElement().getText());

        // Test the constructor with components
        Constructor<?> containerConstructor = findVarargsConstructor(elementClass);
        if (containerConstructor == null) {
          fail("No constructor with varargs of type Component found for " + elementClass);
        }

        Div firstMock = new Div();
        Div secondMock = new Div();
        Component[] componentsArray = {firstMock, secondMock};
        // Pass the varargs array as part of an Object array
        Object[] constructorArgs = new Object[] {componentsArray};
        HtmlComponentContainer<?> container =
            (HtmlComponentContainer<?>) containerConstructor.newInstance(constructorArgs);
        assertEquals(2, container.getComponents().size());
      }
    }
  }

  Constructor<?> findVarargsConstructor(Class<?> clazz) {
    Constructor<?>[] constructors = clazz.getDeclaredConstructors();
    for (Constructor<?> constructor : constructors) {
      Class<?>[] parameterTypes = constructor.getParameterTypes();

      if (parameterTypes.length > 0
          && (parameterTypes[parameterTypes.length - 1].isAssignableFrom(Component[].class))) {
        return constructor;
      }
    }

    return null;
  }
}
