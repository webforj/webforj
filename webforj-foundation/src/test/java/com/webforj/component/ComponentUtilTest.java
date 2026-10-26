package com.webforj.component;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.event.BBjWebEventOptions;
import com.basis.bbj.proxies.sysgui.BBjWebComponent;
import com.basis.startup.type.BBjException;
import com.webforj.component.button.Button;
import com.webforj.component.button.event.ButtonClickEvent;
import com.webforj.component.element.Element;
import com.webforj.component.element.ElementComposite;
import com.webforj.component.element.PropertyDescriptor;
import com.webforj.component.element.annotation.EventName;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.event.ComponentEvent;
import com.webforj.component.field.TextField;
import com.webforj.component.optioninput.RadioButtonGroup;
import com.webforj.component.optioninput.event.RadioButtonGroupChangeEvent;
import com.webforj.conceiver.ConceiverProvider;
import com.webforj.conceiver.DefaultConceiver;
import com.webforj.data.event.ValueChangeEvent;
import com.webforj.dispatcher.EventListener;
import java.util.ArrayList;
import java.util.EventObject;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class ComponentUtilTest {

  @Nested
  @DisplayName("fireComponentEvent method")
  class FireComponentEventMethod {

    @Test
    @DisplayName("should deliver the event to the listeners of a component")
    void shouldDeliverEventToComponentListeners() {
      Button button = new Button();
      List<ButtonClickEvent> received = new ArrayList<>();
      button.addClickListener(received::add);
      ButtonClickEvent event = new ButtonClickEvent(button, Map.of("x", 10));

      ComponentUtil.fireComponentEvent(button, event);

      assertEquals(List.of(event), received);
    }

    @Test
    @DisplayName("should deliver the event to the listeners of a radio button group")
    void shouldDeliverEventToRadioButtonGroupListeners() {
      RadioButtonGroup group = new RadioButtonGroup();
      List<RadioButtonGroupChangeEvent> received = new ArrayList<>();
      group.addChangeListener(received::add);
      RadioButtonGroupChangeEvent event = new RadioButtonGroupChangeEvent(group, Map.of());

      ComponentUtil.fireComponentEvent(group, event);

      assertEquals(List.of(event), received);
    }

    @Test
    @DisplayName("should deliver the event to composite listeners and synchronized properties")
    void shouldDeliverEventToCompositeListenersAndSynchronizedProperties()
        throws BBjException, IllegalAccessException {
      BBjWebComponent control = mock(BBjWebComponent.class);
      when(control.newEventOptions()).thenReturn(mock(BBjWebEventOptions.class));
      when(control.setCallback(eq("color-change"), any(), anyString(),
          any(BBjWebEventOptions.class))).thenReturn(1, 2);
      ColorComposite composite = new ColorComposite();
      Element element = (Element) ComponentUtil.getBoundComponent(composite);
      ReflectionUtils.unNullifyControl(element, control);
      composite.synchronizeColor();
      List<ColorChangeEvent> received = new ArrayList<>();
      composite.addColorChangeListener(received::add);
      ColorChangeEvent event = new ColorChangeEvent(composite, Map.of("color", "red"));

      try (MockedStatic<ConceiverProvider> provider = mockStatic(ConceiverProvider.class)) {
        provider.when(ConceiverProvider::getCurrent).thenReturn(new DefaultConceiver());
        ComponentUtil.fireComponentEvent(composite, event);
      }

      assertEquals(1, received.size());
      assertNotSame(event, received.get(0));
      assertSame(composite, received.get(0).getComponent());
      assertEquals(Map.of("color", "red"), received.get(0).getData());
      assertEquals("red", composite.getColor());
    }

    @Test
    @DisplayName("should deliver the event to a component that overrides its event delivery")
    void shouldDeliverEventToComponentOverridingEventDelivery() {
      EventRecordingComponent component = new EventRecordingComponent();
      ComponentEvent<EventRecordingComponent> event = new ComponentEvent<>(component, Map.of());

      ComponentUtil.fireComponentEvent(component, event);

      assertEquals(List.of(event), component.getEvents());
    }

    @Test
    @DisplayName("should ignore an event without listeners")
    void shouldIgnoreEventWithoutListeners() {
      Button button = new Button();
      ButtonClickEvent event = new ButtonClickEvent(button, Map.of());

      assertDoesNotThrow(() -> ComponentUtil.fireComponentEvent(button, event));
    }

    @Test
    @DisplayName("should reject a null component")
    void shouldRejectNullComponent() {
      Component component = null;
      ButtonClickEvent event = new ButtonClickEvent(new Button(), Map.of());

      assertThrows(NullPointerException.class,
          () -> ComponentUtil.fireComponentEvent(component, event));
    }

    @Test
    @DisplayName("should reject a null event")
    void shouldRejectNullEvent() {
      Button button = new Button();
      ButtonClickEvent event = null;

      assertThrows(NullPointerException.class,
          () -> ComponentUtil.fireComponentEvent(button, event));
    }

    @Test
    @DisplayName("should reject an event of another component")
    void shouldRejectEventOfAnotherComponent() {
      Button button = new Button();
      ButtonClickEvent event = new ButtonClickEvent(new Button(), Map.of());

      IllegalArgumentException exception = assertThrows(IllegalArgumentException.class,
          () -> ComponentUtil.fireComponentEvent(button, event));
      assertEquals("ButtonClickEvent belongs to another component. Fire it on the component "
          + "it was created for.", exception.getMessage());
    }

    @Test
    @DisplayName("should deliver an event without an event name to the composite dispatcher")
    void shouldDeliverEventWithoutEventNameToCompositeDispatcher() {
      ColorComposite composite = new ColorComposite();
      List<ResetEvent> received = new ArrayList<>();
      composite.addResetListener(received::add);
      ResetEvent event = new ResetEvent(composite, Map.of());

      ComponentUtil.fireComponentEvent(composite, event);

      assertEquals(List.of(event), received);
    }

    @Test
    @DisplayName("should deliver the event to the listeners of a composite before attach")
    void shouldDeliverEventToCompositeListenersBeforeAttach() {
      ColorComposite composite = new ColorComposite();
      List<ColorChangeEvent> received = new ArrayList<>();
      composite.addColorChangeListener(received::add);
      ColorChangeEvent event = new ColorChangeEvent(composite, Map.of("color", "red"));

      try (MockedStatic<ConceiverProvider> provider = mockStatic(ConceiverProvider.class)) {
        provider.when(ConceiverProvider::getCurrent).thenReturn(new DefaultConceiver());
        ComponentUtil.fireComponentEvent(composite, event);
      }

      assertEquals(1, received.size());
      assertEquals(Map.of("color", "red"), received.get(0).getData());
    }

    @Test
    @DisplayName("should update synchronized properties before the composite is attached")
    void shouldUpdateSynchronizedPropertiesBeforeAttach() {
      ColorComposite composite = new ColorComposite();
      composite.synchronizeColor();
      ColorChangeEvent event = new ColorChangeEvent(composite, Map.of("color", "red"));

      ComponentUtil.fireComponentEvent(composite, event);

      assertEquals("red", composite.getColor());
    }

    @Test
    @DisplayName("should deliver a composite event without data as an event with empty data")
    void shouldDeliverCompositeEventWithoutDataAsEventWithEmptyData() {
      ColorComposite composite = new ColorComposite();
      List<ColorChangeEvent> received = new ArrayList<>();
      composite.addColorChangeListener(received::add);
      ColorChangeEvent event = new ColorChangeEvent(composite, null);

      try (MockedStatic<ConceiverProvider> provider = mockStatic(ConceiverProvider.class)) {
        provider.when(ConceiverProvider::getCurrent).thenReturn(new DefaultConceiver());
        ComponentUtil.fireComponentEvent(composite, event);
      }

      assertEquals(1, received.size());
      assertEquals(Map.of(), received.get(0).getData());
    }

    @Test
    @DisplayName("should deliver an event of the registered class for a subclassed event")
    void shouldDeliverEventOfRegisteredClassForSubclassedEvent() {
      ColorComposite composite = new ColorComposite();
      List<ColorChangeEvent> received = new ArrayList<>();
      composite.addColorChangeListener(received::add);
      DarkColorChangeEvent event = new DarkColorChangeEvent(composite, Map.of("color", "black"));

      try (MockedStatic<ConceiverProvider> provider = mockStatic(ConceiverProvider.class)) {
        provider.when(ConceiverProvider::getCurrent).thenReturn(new DefaultConceiver());
        ComponentUtil.fireComponentEvent(composite, event);
      }

      assertEquals(1, received.size());
      assertEquals(ColorChangeEvent.class, received.get(0).getClass());
      assertSame(composite, received.get(0).getComponent());
      assertEquals(Map.of("color", "black"), received.get(0).getData());
    }

    @Test
    @DisplayName("should deliver a value change event to the value change listeners of a field")
    void shouldDeliverValueChangeEventToFieldValueChangeListeners() {
      TextField field = new TextField();
      List<ValueChangeEvent<String>> received = new ArrayList<>();
      field.addValueChangeListener(received::add);
      ValueChangeEvent<String> event = new ValueChangeEvent<>(field, "changed");

      ComponentUtil.fireComponentEvent(field, event);

      assertEquals(List.of(event), received);
    }

    @Test
    @DisplayName("should deliver a value change event to the composite dispatcher")
    void shouldDeliverValueChangeEventToCompositeDispatcher() {
      ColorComposite composite = new ColorComposite();
      List<ValueChangeEvent<String>> received = new ArrayList<>();
      composite.addValueChangeListener(received::add);
      ValueChangeEvent<String> event = new ValueChangeEvent<>(composite, "red");

      ComponentUtil.fireComponentEvent(composite, event);

      assertEquals(List.of(event), received);
    }

    @Test
    @DisplayName("should reject a value change event of another component")
    void shouldRejectValueChangeEventOfAnotherComponent() {
      TextField field = new TextField();
      ValueChangeEvent<String> event = new ValueChangeEvent<>(new TextField(), "changed");

      IllegalArgumentException exception = assertThrows(IllegalArgumentException.class,
          () -> ComponentUtil.fireComponentEvent(field, event));
      assertEquals("ValueChangeEvent belongs to another component. Fire it on the component "
          + "it was created for.", exception.getMessage());
    }

    @Test
    @DisplayName("should reject a component that delivers no events")
    void shouldRejectComponentWithoutEventDelivery() {
      ComponentMock component = new ComponentMock();
      ComponentEvent<ComponentMock> event = new ComponentEvent<>(component, Map.of());

      IllegalStateException exception = assertThrows(IllegalStateException.class,
          () -> ComponentUtil.fireComponentEvent(component, event));
      assertEquals("ComponentMock delivers no events to listeners. Fire the event on the "
          + "component its listeners are registered on.", exception.getMessage());
    }
  }

  @NodeName("x-color")
  static class ColorComposite extends ElementComposite {
    private final PropertyDescriptor<String> colorProp =
        PropertyDescriptor.property("color", "blue");

    public void synchronizeColor() {
      synchronize(colorProp, "color-change");
    }

    public void addColorChangeListener(EventListener<ColorChangeEvent> listener) {
      addEventListener(ColorChangeEvent.class, listener);
    }

    public void addResetListener(EventListener<ResetEvent> listener) {
      getEventDispatcher().addListener(ResetEvent.class, listener);
    }

    public void addValueChangeListener(EventListener<ValueChangeEvent<String>> listener) {
      getEventDispatcher().addListener(ValueChangeEvent.class, listener);
    }

    public String getColor() {
      return get(colorProp);
    }
  }

  @EventName("color-change")
  static class ColorChangeEvent extends ComponentEvent<ColorComposite> {
    ColorChangeEvent(ColorComposite component, Map<String, Object> data) {
      super(component, data);
    }
  }

  static class DarkColorChangeEvent extends ColorChangeEvent {
    DarkColorChangeEvent(ColorComposite component, Map<String, Object> data) {
      super(component, data);
    }
  }

  static class ResetEvent extends ComponentEvent<ColorComposite> {
    ResetEvent(ColorComposite component, Map<String, Object> data) {
      super(component, data);
    }
  }

  static class EventRecordingComponent extends ComponentMock {
    private final List<EventObject> events = new ArrayList<>();

    List<EventObject> getEvents() {
      return events;
    }

    @Override
    protected void fireComponentEvent(EventObject event) {
      events.add(event);
    }
  }
}
