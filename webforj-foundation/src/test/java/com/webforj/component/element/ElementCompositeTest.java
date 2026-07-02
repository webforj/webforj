package com.webforj.component.element;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.google.gson.Gson;
import com.webforj.component.element.annotation.EventName;
import com.webforj.component.element.event.ElementEvent;
import com.webforj.component.element.event.ElementEventOptions;
import com.webforj.component.event.ComponentEvent;
import com.webforj.conceiver.ConceiverProvider;
import com.webforj.conceiver.DefaultConceiver;
import com.webforj.dispatcher.EventDispatcher;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Collections;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.DoubleAccumulator;
import java.util.concurrent.atomic.DoubleAdder;
import java.util.concurrent.atomic.LongAccumulator;
import java.util.concurrent.atomic.LongAdder;
import java.util.stream.Stream;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.MockedStatic;

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

    @ParameterizedTest
    @MethodSource("primitivePropertyProvider")
    @DisplayName("should get primitive properties and attributes without adding a Type")
    void shouldGetPrimitivePropertiesAndAttributes(PropertyDescriptor<?> prop, Object defaultValue,
        Object expectedValue) {
      composite.set(prop);
      Object result = composite.get(prop);
      assertEquals(expectedValue, result);
    }

    static Stream<Arguments> simpleAttributeConversionProvider() {
      return Stream.of(
          Arguments.of(PropertyDescriptor.attribute("stringAttr", "hello"), "hello", String.class),
          Arguments.of(PropertyDescriptor.attribute("booleanAttr", true), true, Boolean.class),
          Arguments.of(PropertyDescriptor.attribute("byteAttr", (byte) 5), (byte) 5, Byte.class),
          Arguments.of(PropertyDescriptor.attribute("shortAttr", (short) 42), (short) 42,
              Short.class),
          Arguments.of(PropertyDescriptor.attribute("intAttr", 100), 100, Integer.class),
          Arguments.of(PropertyDescriptor.attribute("longAttr", 9_000_000_000L), 9_000_000_000L,
              Long.class),
          Arguments.of(PropertyDescriptor.attribute("floatAttr", 3.14f), 3.14f, Float.class),
          Arguments.of(PropertyDescriptor.attribute("doubleAttr", 2.718d), 2.718d, Double.class));
    }

    @ParameterizedTest
    @MethodSource("simpleAttributeConversionProvider")
    @DisplayName("should convert simple attribute string values back to their declared type")
    void shouldConvertSimpleAttributeValues(PropertyDescriptor<?> attribute, Object expectedValue,
        Class<?> expectedType) {
      composite.set(attribute);

      Object result = composite.get(attribute);

      assertEquals(expectedValue, result);
      assertEquals(expectedType, result.getClass());
    }

    static Stream<Arguments> numberProvider() {
      return Stream.of(
          Arguments.of(PropertyDescriptor.attribute("atomicInteger", new AtomicInteger(42)),
              new AtomicInteger(42), AtomicInteger.class),
          Arguments.of(PropertyDescriptor.attribute("atomicLong", new AtomicLong(123L)),
              new AtomicLong(123L), AtomicLong.class),
          Arguments.of(PropertyDescriptor.attribute("bigDecimal", new BigDecimal("123.456")),
              new BigDecimal("123.456"), BigDecimal.class),
          Arguments.of(PropertyDescriptor.attribute("bigInteger", new BigInteger("123456789")),
              new BigInteger("123456789"), BigInteger.class),
          Arguments.of(
              PropertyDescriptor.attribute("doubleAccumulator",
                  new DoubleAccumulator(Double::sum, 1.1)),
              new DoubleAccumulator(Double::sum, 1.1), DoubleAccumulator.class),
          Arguments.of(PropertyDescriptor.attribute("doubleAdder", new DoubleAdder()),
              new DoubleAdder(), DoubleAdder.class),
          Arguments.of(
              PropertyDescriptor.attribute("longAccumulator", new LongAccumulator(Long::sum, 10L)),
              new LongAccumulator(Long::sum, 10L), LongAccumulator.class),
          Arguments.of(PropertyDescriptor.attribute("longAdder", new LongAdder()), new LongAdder(),
              LongAdder.class));
    }

    @ParameterizedTest
    @MethodSource("numberProvider")
    void shouldHandleExtendedNumberTypesAsAttributes(PropertyDescriptor<?> attribute,
        Object expectedValue, Class<?> expectedType) {
      composite.set(attribute);
      Object result = composite.get(attribute);

      assertEquals(expectedType, result.getClass());
      if (result instanceof AtomicInteger) {
        assertEquals(((AtomicInteger) expectedValue).get(), ((AtomicInteger) result).get());
      } else if (result instanceof AtomicLong) {
        assertEquals(((AtomicLong) expectedValue).get(), ((AtomicLong) result).get());
      } else if (result instanceof DoubleAccumulator) {
        assertEquals(((DoubleAccumulator) expectedValue).get(), ((DoubleAccumulator) result).get());
      } else if (result instanceof DoubleAdder) {
        assertEquals(((DoubleAdder) expectedValue).sum(), ((DoubleAdder) result).sum());
      } else if (result instanceof LongAccumulator) {
        assertEquals(((LongAccumulator) expectedValue).get(), ((LongAccumulator) result).get());
      } else if (result instanceof LongAdder) {
        assertEquals(((LongAdder) expectedValue).sum(), ((LongAdder) result).sum());
      } else {
        assertEquals(expectedValue, result);
      }
    }

    @ParameterizedTest
    @MethodSource("numberProvider")
    void shouldHandleExtendedNumberTypesAsProperties(PropertyDescriptor<?> property,
        Object expectedValue, Class<?> expectedType) {
      PropertyDescriptor<Object> prop =
          PropertyDescriptor.property(property.getName(), expectedValue);
      composite.set(prop, expectedValue);
      Object result = composite.get(prop);

      assertEquals(expectedValue, result);
      assertEquals(expectedType, result.getClass());
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

    MockedStatic<ConceiverProvider> mockedConceiverProvider;

    @BeforeEach
    void setUp() {
      composite = new ElementCompositeMock();
      mockedConceiverProvider = mockStatic(ConceiverProvider.class);
      when(ConceiverProvider.getCurrent()).thenReturn(new DefaultConceiver());
    }

    @AfterEach
    void tearDown() {
      mockedConceiverProvider.close();
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
