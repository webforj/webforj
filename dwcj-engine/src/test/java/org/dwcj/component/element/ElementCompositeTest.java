package org.dwcj.component.element;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.google.gson.Gson;
import java.util.Collections;
import java.util.Map;
import java.util.stream.Stream;
import org.dwcj.component.element.annotation.EventName;
import org.dwcj.component.element.event.ElementEvent;
import org.dwcj.component.element.event.ElementEventOptions;
import org.dwcj.component.event.ComponentEvent;
import org.dwcj.dispatcher.EventDispatcher;
import org.dwcj.dispatcher.EventListener;
import org.dwcj.dispatcher.ListenerRegistration;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class ElementCompositeTest {

  private ElementCompositeMock composite;

  @BeforeEach
  void setUp() {
    composite = new ElementCompositeMock();
  }

  @Nested
  @DisplayName("Properties API")
  class PropertiesApi {

    class User {
      private String fname;
      private String lname;

      public User(String fname, String lname) {
        this.fname = fname;
        this.lname = lname;
      }

      public String getFname() {
        return fname;
      }

      public String getLname() {
        return lname;
      }

      public boolean equals(Object o) {
        if (o == this) {
          return true;
        }

        if (!(o instanceof User)) {
          return false;
        }

        User user = (User) o;

        return user.fname.equals(fname) && user.lname.equals(lname);
      }
    }

    @Test
    @DisplayName("should set properties and attributes on the element")
    void shouldSetPropertiesAndAttributes() {
      User johnDoe = new User("John", "Doe");
      PropertyDescriptor<User> prop = PropertyDescriptor.property("user", johnDoe);

      composite.set(prop);
      User userFromProperty = composite.getElement().getProperty("user", User.class);
      assertEquals(johnDoe, userFromProperty);
      assertNotSame(johnDoe, userFromProperty);

      PropertyDescriptor<User> attr = PropertyDescriptor.attribute("user", johnDoe);

      composite.set(attr);
      String userFromAttribute = composite.getElement().getAttribute("user");
      assertEquals(new Gson().toJson(johnDoe), userFromAttribute);
      assertEquals(johnDoe, new Gson().fromJson(userFromAttribute, User.class));
    }

    @Test
    @DisplayName("can get properties and attributes from server cache")
    void shouldGetPropertiesFromCache() {
      User johnDoe = new User("John", "Doe");
      PropertyDescriptor<User> prop = PropertyDescriptor.property("user", johnDoe);
      PropertyDescriptor<User> attr = PropertyDescriptor.attribute("user", johnDoe);

      composite.set(prop);
      assertEquals(johnDoe, composite.get(prop));

      composite.set(attr);
      assertEquals(johnDoe, composite.get(attr));
    }

    @Test
    @DisplayName("should get properties and attributes from the client")
    void shouldGetPropertiesAndAttributesFromClient() {
      User johnDoe = new User("John", "Doe");
      Element el = spy(composite.getElement());
      when(el.getProperty("user", User.class)).thenReturn(johnDoe);
      when(el.getAttribute("user")).thenReturn(new Gson().toJson(johnDoe));

      PropertyDescriptor<User> prop = PropertyDescriptor.property("user", johnDoe);
      PropertyDescriptor<User> attr = PropertyDescriptor.attribute("user", johnDoe);

      composite.set(prop);
      assertEquals(johnDoe, composite.get(prop, true));

      composite.set(attr);
      assertEquals(johnDoe, composite.get(attr, true));
    }

    @ParameterizedTest
    @MethodSource("primitivePropertyProvider")
    @DisplayName("should get primitive properties and attributes without adding a Type")
    void shouldGetPrimitivePropertiesAndAttributes(PropertyDescriptor<?> prop, Object defaultValue,
        Object expectedValue) {
      composite.set(prop);
      Object result = composite.get(prop);
      assertEquals(expectedValue, result);
    }

    static Stream<Arguments> primitivePropertyProvider() {
      return Stream.of(
          // PropertyDescriptor.property cases
          Arguments.of(PropertyDescriptor.property("intProperty", 42), 0, 42),
          Arguments.of(PropertyDescriptor.property("doubleProperty", 3.14), 0.0, 3.14),
          Arguments.of(PropertyDescriptor.property("booleanProperty", true), false, true),
          Arguments.of(PropertyDescriptor.property("stringProperty", "Hello"), null, "Hello"),

          // PropertyDescriptor.attribute cases
          Arguments.of(PropertyDescriptor.attribute("intAttribute", "100"), "0", "100"),
          Arguments.of(PropertyDescriptor.attribute("doubleAttribute", "2.718"), "0.0", "2.718"),
          Arguments.of(PropertyDescriptor.attribute("booleanAttribute", "false"), "true", "false"),
          Arguments.of(PropertyDescriptor.attribute("stringAttribute", "World"), null, "World"));
    }
  }

  @Nested
  @DisplayName("Events API")
  class EventsApi {

    @EventName("click")
    static class ClickEvent extends ComponentEvent<ElementCompositeMock> {
      public ClickEvent(ElementCompositeMock component, Map<String, Object> eventMap) {
        super(component, eventMap);
      }
    }

    @Test
    @DisplayName("should throw an an exception if event is already mapped to a class")
    void shouldThrowExceptionIfEventIsMapped() {
      @EventName("click")
      class ClickEvent2 extends ComponentEvent<ElementCompositeMock> {
        public ClickEvent2(ElementCompositeMock component, Map<String, Object> eventMap) {
          super(component, eventMap);
        }
      }

      composite.addEventListener(ClickEvent.class, e -> {
        // pass
      });

      assertThrows(IllegalStateException.class, () -> {
        composite.addEventListener(ClickEvent2.class, e -> {
          // pass
        });
      });
    }

    @Test
    @DisplayName("should dispatch custom events")
    void shouldDispatchCustomEvents() {
      // mock element
      Element el = mock(Element.class);
      ListenerRegistration<ElementEvent> elRegistrationMock = mock(ListenerRegistration.class);
      when(el.addEventListener(anyString(), any(EventListener.class),
          any(ElementEventOptions.class), anyBoolean())).thenReturn(elRegistrationMock);

      // mock composite
      ElementCompositeMock compositeSpy = spy(composite);
      when(compositeSpy.getElement()).thenReturn(el);
      EventListener<ClickEvent> listenerMock = mock(EventListener.class);
      compositeSpy.addEventListener(ClickEvent.class, listenerMock);

      ElementEvent mockElementEvent = mock(ElementEvent.class);
      when(mockElementEvent.getType()).thenReturn("click");
      when(mockElementEvent.getEventMap()).thenReturn(Collections.emptyMap());

      compositeSpy.handleEvent(mockElementEvent, elRegistrationMock);

      verify(compositeSpy, times(1)).createEvent(eq(ClickEvent.class), anyMap());
      verify(listenerMock, times(1)).onEvent(any(ClickEvent.class));
    }

    @Test
    @DisplayName("removing the listener will remove the Element listener")
    void shouldRemoveElementListener() {
      EventListener<ElementEvent> listener = mock(EventListener.class);

      ListenerRegistration<ElementEvent> registration = mock(ListenerRegistration.class);
      when(registration.getListener()).thenReturn(listener);
      when((Object) registration.getEventClass()).thenReturn(ElementEvent.class);

      ListenerRegistration<ElementEvent> elementRegistration = mock(ListenerRegistration.class);
      when(elementRegistration.getListener()).thenReturn(listener);
      when((Object) elementRegistration.getEventClass()).thenReturn(ElementEvent.class);

      ElementComposite.ElementCompositeListenerRegistration<?> listenerRegistration =
          composite.new ElementCompositeListenerRegistration<>(registration, elementRegistration,
              new EventDispatcher());

      listenerRegistration.remove();
      verify(elementRegistration, times(1)).remove();
    }

  }
}
