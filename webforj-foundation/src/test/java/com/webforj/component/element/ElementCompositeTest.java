package com.webforj.component.element;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.event.BBjWebEvent;
import com.basis.bbj.proxies.event.BBjWebEventOptions;
import com.basis.bbj.proxies.sysgui.BBjWebComponent;
import com.basis.startup.type.BBjException;
import com.google.gson.Gson;
import com.webforj.component.ReflectionUtils;
import com.webforj.component.element.annotation.EventName;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.element.annotation.Synchronize;
import com.webforj.component.element.event.ElementEvent;
import com.webforj.component.element.event.ElementEventOptions;
import com.webforj.component.element.sink.ElementEventSink;
import com.webforj.component.event.ComponentEvent;
import com.webforj.component.window.Window;
import com.webforj.conceiver.ConceiverProvider;
import com.webforj.conceiver.DefaultConceiver;
import com.webforj.dispatcher.EventDispatcher;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.exceptions.WebforjRuntimeException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Stream;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.ArgumentCaptor;
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
          Arguments.of(PropertyDescriptor.attribute("doubleAttr", 2.718d), 2.718d, Double.class),
          Arguments.of(PropertyDescriptor.attribute("bigDecimalAttr", new BigDecimal("123.456")),
              new BigDecimal("123.456"), BigDecimal.class),
          Arguments.of(PropertyDescriptor.attribute("bigIntegerAttr", new BigInteger("123456789")),
              new BigInteger("123456789"), BigInteger.class));
    }

    @Test
    @DisplayName("should convert simple attribute values read from the client")
    void shouldConvertSimpleAttributeValuesFromClient() {
      PropertyDescriptor<Integer> attr = PropertyDescriptor.attribute("count", 0);
      composite.set(attr, 42);

      assertEquals(42, composite.get(attr, true));
    }

    @Test
    @DisplayName("should read a present boolean attribute as true regardless of its value")
    void shouldReadPresentBooleanAttributeAsTrue() {
      PropertyDescriptor<Boolean> attr = PropertyDescriptor.attribute("disabled", false);
      composite.getElement().setAttribute("disabled", "");

      assertTrue(composite.get(attr, true));
    }

    @Test
    @DisplayName("should read an absent boolean attribute as false")
    void shouldReadAbsentBooleanAttributeAsFalse() {
      PropertyDescriptor<Boolean> attr = PropertyDescriptor.attribute("disabled", false);

      assertFalse(composite.get(attr, true));
    }

    @Test
    @DisplayName("should read a present boolean attribute with a \"false\" value as false")
    void shouldReadFalseStringBooleanAttributeAsFalse() {
      PropertyDescriptor<Boolean> attr = PropertyDescriptor.attribute("disabled", false);
      composite.getElement().setAttribute("disabled", "false");

      assertFalse(composite.get(attr, true));
    }

    @Test
    @DisplayName("should add the attribute for a true value and remove it for a false value")
    void shouldWriteBooleanAttributeByPresence() {
      PropertyDescriptor<Boolean> attr = PropertyDescriptor.attribute("disabled", false);

      composite.set(attr, true);
      assertTrue(composite.getElement().hasAttribute("disabled"));
      assertTrue(composite.get(attr));

      composite.set(attr, false);
      assertFalse(composite.getElement().hasAttribute("disabled"));
      assertFalse(composite.get(attr));
    }

    @Test
    @DisplayName("should return the default value when the client value cannot be parsed")
    void shouldFallBackToDefaultWhenClientValueIsNotParseable() {
      PropertyDescriptor<Integer> attr = PropertyDescriptor.attribute("count", 7);
      composite.set(attr, 100);
      composite.getElement().setAttribute("count", "abc");

      assertEquals(7, composite.get(attr, true));
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

  @Nested
  @DisplayName("Synchronize API")
  class SynchronizeApi {

    @NodeName("x-color")
    static class SynchronizedColorComposite extends ElementComposite {
      @Synchronize("change")
      final PropertyDescriptor<String> colorProp = PropertyDescriptor.property("color", "blue");

      @Synchronize("toggle")
      final PropertyDescriptor<String> badgeAttr = PropertyDescriptor.attribute("badge", "none");

      public String getColor() {
        return get(colorProp);
      }

      public String getBadge() {
        return get(badgeAttr);
      }
    }

    @NodeName("x-detail-color")
    static class SynchronizedDetailColorComposite extends ElementComposite {
      @Synchronize(value = "change", exp = "event.detail")
      final PropertyDescriptor<String> colorProp = PropertyDescriptor.property("color", "blue");

      public String getColor() {
        return get(colorProp);
      }
    }

    @NodeName("x-checked")
    static class SynchronizedCheckedComposite extends ElementComposite {
      @Synchronize("toggle")
      final PropertyDescriptor<Boolean> checkedAttr =
          PropertyDescriptor.attribute("checked", false);

      public boolean isChecked() {
        return get(checkedAttr);
      }
    }

    @NodeName("x-count")
    static class SynchronizedCountComposite extends ElementComposite {
      final PropertyDescriptor<Integer> countProp = PropertyDescriptor.property("count", 0);

      public void synchronizeCount() {
        synchronize(countProp, "input");
      }

      public int getCount() {
        return get(countProp);
      }
    }

    @NodeName("x-broken")
    static class BrokenSynchronizedComposite extends ElementComposite {
      @Synchronize("change")
      final String colorProp = "color";
    }

    static class BaseSynchronizedComposite extends ElementComposite {
      @Synchronize("change")
      final PropertyDescriptor<String> colorProp = PropertyDescriptor.property("color", "blue");

      public String getColor() {
        return get(colorProp);
      }
    }

    @NodeName("x-inherited-color")
    static class InheritedSynchronizedComposite extends BaseSynchronizedComposite {
    }

    BBjWebComponent control;
    BBjWebEventOptions controlOptions;

    @BeforeEach
    void setUpControl() throws BBjException {
      control = mock(BBjWebComponent.class);
      controlOptions = mock(BBjWebEventOptions.class);
      when(control.newEventOptions()).thenReturn(controlOptions);
    }

    ElementEventSink attachAndCaptureSink(ElementComposite composite, String event, int callbackId)
        throws BBjException, IllegalAccessException {
      when(control.setCallback(eq(event), any(), anyString(), any(BBjWebEventOptions.class)))
          .thenReturn(callbackId);

      Element element = composite.getElement();
      ReflectionUtils.unNullifyControl(element, control);
      element.attachControlCallbacks();

      ArgumentCaptor<Object> handler = ArgumentCaptor.forClass(Object.class);
      verify(control).setCallback(eq(event), handler.capture(), anyString(),
          any(BBjWebEventOptions.class));

      return (ElementEventSink) handler.getValue();
    }

    BBjWebEvent webEvent(int callbackId, Map<String, Object> eventMap) {
      BBjWebEvent event = mock(BBjWebEvent.class);
      when(event.getCallbackID()).thenReturn(callbackId);
      when(event.getEventMap()).thenReturn(eventMap);

      return event;
    }

    @Test
    @DisplayName("should sync annotated property with the client")
    void shouldSyncAnnotatedProperty() throws BBjException, IllegalAccessException {
      SynchronizedColorComposite composite = new SynchronizedColorComposite();
      composite.onCreate(mock(Window.class));

      ElementEventSink sink = attachAndCaptureSink(composite, "change", 1);
      verify(controlOptions).addItem("color", "component['color']");

      assertEquals("blue", composite.getColor());
      sink.handleEvent(webEvent(1, Map.of("color", "red")));
      assertEquals("red", composite.getColor());
    }

    @Test
    @DisplayName("should sync annotated attribute with the client")
    void shouldSyncAnnotatedAttribute() throws BBjException, IllegalAccessException {
      SynchronizedColorComposite composite = new SynchronizedColorComposite();
      composite.onCreate(mock(Window.class));

      ElementEventSink sink = attachAndCaptureSink(composite, "toggle", 2);
      verify(controlOptions).addItem("badge", "component.getAttribute('badge')");

      assertEquals("none", composite.getBadge());
      sink.handleEvent(webEvent(2, Map.of("badge", "new")));
      assertEquals("new", composite.getBadge());
    }

    @Test
    @DisplayName("should sync boolean attribute presence with the client")
    void shouldSyncBooleanAttribute() throws BBjException, IllegalAccessException {
      SynchronizedCheckedComposite composite = new SynchronizedCheckedComposite();
      composite.onCreate(mock(Window.class));

      ElementEventSink sink = attachAndCaptureSink(composite, "toggle", 3);

      assertFalse(composite.isChecked());
      sink.handleEvent(webEvent(3, Map.of("checked", "")));
      assertTrue(composite.isChecked());
    }

    @Test
    @DisplayName("should sync from the given expression and write the value back to the element")
    void shouldSyncFromExpression() throws BBjException, IllegalAccessException {
      SynchronizedDetailColorComposite composite = new SynchronizedDetailColorComposite();
      composite.onCreate(mock(Window.class));

      ElementEventSink sink = attachAndCaptureSink(composite, "change", 4);
      verify(controlOptions).addItem("color", "event.detail");
      verify(controlOptions).setCode("component['color'] = event.detail;");

      sink.handleEvent(webEvent(4, Map.of("color", "green")));
      assertEquals("green", composite.getColor());
    }

    @Test
    @DisplayName("should sync programmatically and convert the reported value to the property type")
    void shouldSyncProgrammatically() throws BBjException, IllegalAccessException {
      SynchronizedCountComposite composite = new SynchronizedCountComposite();
      composite.synchronizeCount();

      ElementEventSink sink = attachAndCaptureSink(composite, "input", 5);
      sink.handleEvent(webEvent(5, Map.of("count", 4.0d)));

      assertEquals(4, composite.getCount());
    }

    @Test
    @DisplayName("should keep the cached value when the event carries no value")
    void shouldKeepCachedValueWhenEventCarriesNoValue()
        throws BBjException, IllegalAccessException {
      SynchronizedColorComposite composite = new SynchronizedColorComposite();
      composite.onCreate(mock(Window.class));

      ElementEventSink sink = attachAndCaptureSink(composite, "change", 6);
      sink.handleEvent(webEvent(6, Map.of()));

      assertEquals("blue", composite.getColor());
    }

    @Test
    @DisplayName("should throw when the annotated field is not a PropertyDescriptor")
    void shouldThrowWhenAnnotatedFieldIsNotDescriptor() {
      BrokenSynchronizedComposite composite = new BrokenSynchronizedComposite();
      Window window = mock(Window.class);

      assertThrows(WebforjRuntimeException.class, () -> composite.onCreate(window));
    }

    @Test
    @DisplayName("should sync a property annotated on a superclass field")
    void shouldSyncInheritedAnnotatedProperty() throws BBjException, IllegalAccessException {
      InheritedSynchronizedComposite composite = new InheritedSynchronizedComposite();
      composite.onCreate(mock(Window.class));

      ElementEventSink sink = attachAndCaptureSink(composite, "change", 7);
      verify(controlOptions).addItem("color", "component['color']");

      assertEquals("blue", composite.getColor());
      sink.handleEvent(webEvent(7, Map.of("color", "red")));
      assertEquals("red", composite.getColor());
    }
  }
}
