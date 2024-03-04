package com.webforj.component.element;

import com.google.gson.Gson;
import com.webforj.component.Component;
import com.webforj.component.Composite;
import com.webforj.component.element.annotation.ElementAnnotationProcessor;
import com.webforj.component.element.annotation.EventName;
import com.webforj.component.element.event.ElementEvent;
import com.webforj.component.element.event.ElementEventOptions;
import com.webforj.component.event.ComponentEvent;
import com.webforj.dispatcher.EventDispatcher;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.exceptions.DwcjRuntimeException;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Type;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicReference;

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
    return dispatcher.getListeners(eventClass);
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

      // Attribute values are serialized according to value's current type (regardless of the
      // property's type value):
      //
      // 1. Strings, Numbers and Booleans No serialization required. The value is converted to a its
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
   * @param the property descriptor
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
   * @throws DwcjRuntimeException if the event class is not annotated with {@link EventName}
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
    ListenerRegistration<E> registration = dispatcher.addListener(eventClass, listener);
    return new ElementCompositeListenerRegistration<>(registration, elementRegistration,
        dispatcher);
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
   * @throws DwcjRuntimeException if the event class is not annotated with {@link EventName}
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
      ComponentEvent<?> event = createEvent(eventNameToClassMap.get(eventName), eventMap);
      EventListener<? extends ComponentEvent<?>> listener =
          listenerRegistrations.get(originalRegistration);

      if (event != null && listener != null) {
        dispatcher.dispatchEvent(event, (l, e) -> l == listener);
      }
    }
  }

  /**
   * Creates a custom event instance of the specified event class with the provided data.
   *
   * @param <E> The type of the event to create.
   * @param eventClass The class of the event to create.
   * @param data A map of data to initialize the event with.
   *
   * @return An instance of the specified event class.
   * @throws DwcjRuntimeException if the event class cannot be instantiated or initialized properly.
   */
  <E extends ComponentEvent<?>> E createEvent(Class<?> eventClass, Map<String, Object> data) {
    E event = null;

    try {
      Constructor<?>[] constructors = eventClass.getDeclaredConstructors();
      for (Constructor<?> constructor : constructors) {
        constructor.setAccessible(true); // NOSONAR
        Class<?>[] parameterTypes = constructor.getParameterTypes();

        // is not inner class
        if (parameterTypes.length == 2 && Component.class.isAssignableFrom(parameterTypes[0])
            && parameterTypes[1] == Map.class) {
          event = (E) constructor.newInstance(this, data); // NOSONAR
          break;
        }
        // else if inner class
        else if (parameterTypes.length == 3 && Component.class.isAssignableFrom(parameterTypes[0])
            && Component.class.isAssignableFrom(parameterTypes[1])
            && parameterTypes[2] == Map.class) {
          event = (E) constructor.newInstance(this, this, data); // NOSONAR

          break;
        }
      }
    } catch (InstantiationException | IllegalAccessException | IllegalArgumentException
        | InvocationTargetException e) {
      throw new DwcjRuntimeException(
          "Failed to instantiate the event class " + eventClass.getSimpleName(), e);
    }

    return event;
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
    String name = property.getName();
    String value = fromClient ? getElement().getAttribute(name) : attributes.get(name);

    if (value != null) {
      if (isSimpleType(type)) {
        return (V) value;
      } else {
        Gson gson = new Gson();
        return gson.fromJson(value, type);
      }
    } else {
      return property.getDefaultValue();
    }
  }

  /**
   * Checks if a given type is a simple type (String, Boolean, or a Number).
   *
   * @param type the type to check
   * @return true if the type is a simple type, false otherwise
   */
  private boolean isSimpleType(Type type) {
    return type.equals(String.class) || type.equals(Boolean.class)
        || Number.class.isAssignableFrom((Class<?>) type);
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
