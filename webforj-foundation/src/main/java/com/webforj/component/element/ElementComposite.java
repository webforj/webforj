package com.webforj.component.element;

import com.google.gson.Gson;
import com.webforj.component.Composite;
import com.webforj.component.element.annotation.ElementAnnotationProcessor;
import com.webforj.component.element.annotation.EventName;
import com.webforj.component.element.event.ElementEvent;
import com.webforj.component.element.event.ElementEventOptions;
import com.webforj.component.event.ComponentEvent;
import com.webforj.conceiver.Conceiver;
import com.webforj.conceiver.ConceiverProvider;
import com.webforj.dispatcher.EventDispatcher;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.exceptions.WebforjRuntimeException;
import java.lang.reflect.Type;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;

/**
 * The {@code ElementComposite} class serves as an abstract base class for creating composite
 * elements. This class extends the {@link Composite} class, specifically tailored to handle HTML
 * {@link Element} instances, providing a convenient way to manage properties, attributes, and event
 * listeners of these elements.
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 *
 * @see Composite
 * @see Element
 */
public abstract class ElementComposite extends Composite<Element> {
  private final Map<String, String> attributes = new HashMap<>();
  private final Map<String, Class<?>> attributeTypes = new HashMap<>();
  private final Map<String, Object> properties = new HashMap<>();
  private final Map<String, Class<?>> propertyTypes = new HashMap<>();
  private final EventDispatcher dispatcher = new EventDispatcher();
  private final HashMap<String, Class<?>> eventNameToClassMap = new HashMap<>();
  private final Map<ListenerRegistration<ElementEvent>, EventListener<? extends ComponentEvent<?>>> listenerRegistrations =
      new HashMap<>();

  private static final Map<Class<?>, Function<String, Object>> SIMPLE_CONVERTERS =
      Map.ofEntries(Map.entry(String.class, s -> s), Map.entry(Byte.class, Byte::valueOf),
          Map.entry(Short.class, Short::valueOf), Map.entry(Integer.class, Integer::valueOf),
          Map.entry(Long.class, Long::valueOf), Map.entry(Float.class, Float::valueOf),
          Map.entry(Double.class, Double::valueOf), Map.entry(BigDecimal.class, BigDecimal::new),
          Map.entry(BigInteger.class, BigInteger::new));

  /**
   * Gets the listeners for the given event class.
   *
   * @param <E> the generic type
   * @param eventClass the event class
   *
   * @return the listeners
   */
  public final <E extends ComponentEvent<?>> List<EventListener<E>> getEventListeners(
      Class<E> eventClass) {
    return getEventDispatcher().getListeners(eventClass);
  }

  /**
   * Called by the {@link #initBoundComponent} method to provide the {@link Element} bound
   * component.
   *
   * @return An instance of the bound {@code Element} associated with this {@code Composite}
   */
  @Override
  protected final Element initBoundComponent() {
    Element element = super.getBoundComponent();
    if (element == null) {
      return new Element(getNodeName());
    }

    return element;
  }

  /**
   * Gets the node name of the element.
   *
   * @return the node name
   */
  protected String getNodeName() {
    return ElementAnnotationProcessor.processNodeName(getClass());
  }

  /**
   * Alias for {@link #getBoundComponent()}.
   *
   * <p>
   * Gets the underlying {@link Element} instance.
   * </p>
   *
   * @return the element instance.
   */
  protected Element getElement() {
    return getBoundComponent();
  }

  /**
   * Sets a property or an attribute of the element.
   *
   * @param <V> the type of the property
   * @param property the property
   * @param value the value of the property
   */
  protected <V> void set(PropertyDescriptor<V> property, V value) {
    boolean isAttribute = property.isAttribute();
    String name = property.getName();

    if (!isAttribute) {
      getElement().setProperty(name, value);
      properties.put(name, value);

      // keep track of the property types
      propertyTypes.put(name, value.getClass());
    } else {
      // keep track of the attribute types
      attributeTypes.put(name, value.getClass());

      if (value instanceof Boolean booleanValue) {
        // Boolean attributes follow the HTML contract: a true value adds the attribute with an
        // empty string, a false value removes it entirely.
        writeBooleanAttribute(name, booleanValue);

        return;
      }

      // Attribute values are serialized according to value's current type (regardless of the
      // property's type value):
      //
      // 1. Strings and Numbers No serialization required. The value is converted to a its
      // string representation using String.valueOf.
      //
      // 2. Everything else The value is serialized using Gson.
      String serializedValue;
      if (isSimpleType(value.getClass())) {
        serializedValue = String.valueOf(value);
      } else {
        Gson gson = new Gson();
        serializedValue = gson.toJson(value);
      }

      getElement().setAttribute(name, serializedValue);
      attributes.put(name, serializedValue);
    }
  }

  /**
   * Sets a property or an attribute of the element.
   *
   * @param <V> the type of the property
   * @param property the property
   */
  protected <V> void set(PropertyDescriptor<V> property) {
    set(property, property.getDefaultValue());
  }

  /**
   * Gets a property or an attribute of the element.
   *
   * @param <V> the type of the property
   * @param property the property descriptor
   * @param fromClient true if the property should be read from the client
   * @param type the type of the property
   *
   * @return The value of the property or attribute or the default value if the property or
   *         attribute is not set.
   */
  protected <V> V get(PropertyDescriptor<V> property, boolean fromClient, Type type) {
    boolean isAttribute = property.isAttribute();
    String name = property.getName();
    Type theType = Optional.ofNullable(type)
        .orElseGet(() -> isAttribute ? attributeTypes.get(name) : propertyTypes.get(name));

    if (theType == null) {
      theType = Object.class;
    }

    if (!isAttribute) {
      return getPropertyValue(property, fromClient, theType);
    } else {
      return getAttributeValue(property, fromClient, theType);
    }
  }

  /**
   * Gets a property or an attribute of the element.
   *
   * @param <V> the type of the property
   * @param property the property descriptor
   * @param fromClient true if the property should be read from the client
   *
   * @return the value of the property or attribute or the default value if the property or
   *         attribute is not set.
   */
  protected <V> V get(PropertyDescriptor<V> property, boolean fromClient) {
    return get(property, fromClient, property.getType());
  }

  /**
   * Gets a property or an attribute of the element.
   *
   * @param <V> the type of the property
   * @param property the property
   *
   * @return the value of the property or attribute or the default value if the property or
   *         attribute is not set.
   */
  protected <V> V get(PropertyDescriptor<V> property) {
    return get(property, false);
  }

  /**
   * Gets the event dispatcher.
   *
   * @return the event dispatcher
   */
  protected EventDispatcher getEventDispatcher() {
    return dispatcher;
  }

  /**
   * Adds a listener for the given event type.
   *
   * @param <E> The event type
   *
   * @param eventClass The event class
   * @param listener The listener
   * @param options The event options
   *
   * @return A listener registration for removing the event listener
   *
   * @throws WebforjRuntimeException if the event class is not annotated with {@link EventName}
   * @throws IllegalStateException if the event name is already registered with a different event
   *         class
   */
  protected <E extends ComponentEvent<?>> ListenerRegistration<E> addEventListener(
      Class<? super E> eventClass, EventListener<E> listener, ElementEventOptions options) {

    String eventName = ElementAnnotationProcessor.processEventName(eventClass);

    // Track the event class for the event name. If the eventName exists and the eventClass is
    // different, throw an exception
    if (eventNameToClassMap.containsKey(eventName)
        && !eventNameToClassMap.get(eventName).equals(eventClass)) {
      throw new IllegalStateException(
          "The event name " + eventName + " is already registered with the event class "
              + eventNameToClassMap.get(eventName).getName());
    } else {
      eventNameToClassMap.put(eventName, eventClass);
    }

    // Prepare the event options. The parse of the options is done in the following order:
    // 1. The options from the event class
    // 2. The options from the listener class
    // 3. The options passed to the method
    ElementEventOptions eventOptionsFromEvent =
        ElementAnnotationProcessor.processEventOptions(eventClass);

    ElementEventOptions eventOptionsFromListener =
        ElementAnnotationProcessor.processEventOptions(listener.getClass());

    ElementEventOptions eventOptions = (new ElementEventOptions()).mergeWith(eventOptionsFromEvent,
        eventOptionsFromListener, options);

    AtomicReference<ListenerRegistration<ElementEvent>> elementRegistrationRef =
        new AtomicReference<>();

    ListenerRegistration<ElementEvent> elementRegistration = getElement().addEventListener(
        eventName, originalEvent -> handleEvent(originalEvent, elementRegistrationRef.get()),
        eventOptions, false);

    elementRegistrationRef.set(elementRegistration);
    listenerRegistrations.put(elementRegistrationRef.get(), listener);

    // We need to wrap the listener registration so that we can remove the listener from the
    // element and the dispatcher.
    ListenerRegistration<E> registration = getEventDispatcher().addListener(eventClass, listener);
    return new ElementCompositeListenerRegistration<>(registration, elementRegistration,
        getEventDispatcher());
  }

  /**
   * Adds a listener for the given event type.
   *
   * @param <E> The event type
   *
   * @param eventClass The event class
   * @param listener The listener
   *
   * @return A listener registration for removing the event listener
   *
   * @throws WebforjRuntimeException if the event class is not annotated with {@link EventName}
   */
  protected <E extends ComponentEvent<?>> ListenerRegistration<E> addEventListener(
      Class<? super E> eventClass, EventListener<E> listener) {
    return addEventListener(eventClass, listener, null);
  }

  /**
   * Handles the given element event and dispatches the custom event.
   *
   * @param originalEvent The original event.
   * @param originalRegistration The original event registration.
   */
  void handleEvent(ElementEvent originalEvent,
      ListenerRegistration<ElementEvent> originalRegistration) {
    Map<String, Object> eventMap = originalEvent.getEventMap();
    String eventName = originalEvent.getType();

    // fire the custom event
    if (eventNameToClassMap.containsKey(eventName)) {
      Conceiver conceiver = ConceiverProvider.getCurrent();
      Class<?> eventClass = eventNameToClassMap.get(eventName);
      ComponentEvent<?> event = conceiver.getComponentEvent(this, eventClass, eventMap);
      EventListener<? extends ComponentEvent<?>> listener =
          listenerRegistrations.get(originalRegistration);

      if (event != null && listener != null) {
        getEventDispatcher().dispatchEvent(event, (l, e) -> l == listener);
      }
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onDidDestroy() {
    super.onDidDestroy();
    attributes.clear();
    attributeTypes.clear();
    properties.clear();
    propertyTypes.clear();
    eventNameToClassMap.clear();
    dispatcher.removeAllListeners();
  }

  /**
   * Gets the value of a property of the element.
   *
   * @param <V> the type of the property
   * @param property the property descriptor
   * @param fromClient true if the property should be read from the client
   * @param type the type of the property
   *
   * @return The value of the property, or the default value if not set.
   */
  private <V> V getPropertyValue(PropertyDescriptor<V> property, boolean fromClient, Type type) {
    String name = property.getName();

    if (fromClient) {
      return getElement().getProperty(name, type);
    } else {
      @SuppressWarnings("unchecked")
      V result = (V) properties.getOrDefault(name, property.getDefaultValue());
      return result;
    }
  }

  /**
   * Gets the value of a attribute of the element.
   *
   * @param <V> the type of the attribute
   * @param property the property descriptor
   * @param fromClient true if the attribute should be read from the client
   * @param type the type of the attribute
   *
   * @return The value of the attribute, or the default value if not set.
   */
  private <V> V getAttributeValue(PropertyDescriptor<V> property, boolean fromClient, Type type) {
    if (Boolean.class.equals(type)) {
      return resolveBooleanAttribute(property, fromClient);
    }

    String name = property.getName();
    String value = fromClient ? getElement().getAttribute(name) : attributes.get(name);

    if (value != null) {
      if (isSimpleType(type)) {
        return convertSimpleAttributeValue(property, value, type);
      } else {
        Gson gson = new Gson();
        return gson.fromJson(value, type);
      }
    } else {
      return property.getDefaultValue();
    }
  }

  /**
   * Converts a simple attribute string value to the declared attribute type.
   *
   * @param <V> the type of the attribute
   * @param property the property descriptor
   * @param value the raw attribute value
   * @param type the declared type of the attribute
   *
   * @return the converted value, or the descriptor default value when the value cannot be parsed as
   *         the declared type
   */
  @SuppressWarnings("unchecked")
  private <V> V convertSimpleAttributeValue(PropertyDescriptor<V> property, String value,
      Type type) {
    if (!(type instanceof Class<?> targetType)) {
      return (V) value;
    }

    Function<String, Object> converter = SIMPLE_CONVERTERS.get(targetType);
    if (converter == null) {
      return (V) value;
    }

    try {
      return (V) converter.apply(value);
    } catch (NumberFormatException e) {
      return property.getDefaultValue();
    }
  }

  /**
   * Writes a {@link Boolean} attribute following the HTML boolean attribute contract.
   *
   * <p>
   * A {@code true} value adds the attribute with an empty string, while a {@code false} value
   * removes the attribute entirely.
   * </p>
   *
   * @see <a href="https://developer.mozilla.org/en-US/docs/Glossary/Boolean/HTML">MDN Boolean
   *      attribute</a>
   *
   * @param name the attribute name
   * @param value the boolean value
   */
  private void writeBooleanAttribute(String name, boolean value) {
    if (value) {
      getElement().setAttribute(name, "");
      attributes.put(name, "");
    } else {
      getElement().removeAttribute(name);
      attributes.remove(name);
    }
  }

  /**
   * Resolves a {@link Boolean} attribute.
   *
   * <p>
   * An absent attribute reads as {@code false} and a present attribute reads as {@code true},
   * following the HTML boolean attribute rule. As the single exception, a present attribute whose
   * value is the literal string {@code "false"} reads as {@code false}, so a component that
   * reflects a false value as that string is honored rather than read as present.
   * </p>
   *
   * @see <a href="https://developer.mozilla.org/en-US/docs/Glossary/Boolean/HTML">MDN Boolean
   *      attribute</a>
   *
   * @param <V> the type of the attribute
   * @param property the property descriptor
   * @param fromClient true if the attribute should be read from the client
   *
   * @return the resolved boolean value
   */
  @SuppressWarnings("unchecked")
  private <V> V resolveBooleanAttribute(PropertyDescriptor<V> property, boolean fromClient) {
    String name = property.getName();
    boolean present = fromClient ? getElement().hasAttribute(name) : attributes.containsKey(name);

    if (!present) {
      return (V) Boolean.FALSE;
    }

    String value = fromClient ? getElement().getAttribute(name) : attributes.get(name);

    return (V) Boolean.valueOf(!"false".equals(value));
  }

  /**
   * Checks if a given type is a simple type. Simple types are String, Byte, Short, Integer, Long,
   * Float, Double, BigDecimal and BigInteger.
   *
   * @param type the type to check
   * @return true if the type is a simple type, false otherwise
   */
  private boolean isSimpleType(Type type) {
    return type instanceof Class<?> targetType && SIMPLE_CONVERTERS.containsKey(targetType);
  }

  /**
   * A custom Element listener registration that will remove the listener from the element and the
   * dispatcher.
   *
   * @author Hyyan Abo Fakher
   */
  final class ElementCompositeListenerRegistration<E extends ComponentEvent<?>>
      extends ListenerRegistration<E> {

    private final ListenerRegistration<ElementEvent> originalRegistration;

    ElementCompositeListenerRegistration(ListenerRegistration<E> registration,
        ListenerRegistration<ElementEvent> elementRegistration, EventDispatcher dispatcher) {
      super(dispatcher, registration.getEventClass(), registration.getListener());
      this.originalRegistration = elementRegistration;
    }

    @Override
    public void remove() {
      super.remove();
      originalRegistration.remove();
    }
  }
}
