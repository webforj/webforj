package com.webforj.component.element;

import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.bbj.proxies.sysgui.BBjWebComponent;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.webforj.PendingResult;
import com.webforj.annotation.ExcludeFromJacocoGeneratedReport;
import com.webforj.bridge.ComponentAccessor;
import com.webforj.bridge.WindowAccessor;
import com.webforj.component.Component;
import com.webforj.component.DwcContainer;
import com.webforj.component.DwcFocusableMixin;
import com.webforj.component.JsExecutor;
import com.webforj.component.element.annotation.ElementAnnotationProcessor;
import com.webforj.component.element.annotation.EventOptions;
import com.webforj.component.element.event.ElementDefinedEvent;
import com.webforj.component.element.event.ElementEvent;
import com.webforj.component.element.event.ElementEventOptions;
import com.webforj.component.element.sink.ElementDefinedEventSink;
import com.webforj.component.element.sink.ElementEventSink;
import com.webforj.component.event.BlurEvent;
import com.webforj.component.event.EventSinkListenerRegistry;
import com.webforj.component.event.ExecuteAsyncScriptEvent;
import com.webforj.component.event.FocusEvent;
import com.webforj.component.event.sink.ExecuteAsyncScriptEventSink;
import com.webforj.component.optioninput.RadioButtonGroup;
import com.webforj.component.window.Window;
import com.webforj.concern.HasEnablement;
import com.webforj.concern.HasFocus;
import com.webforj.concern.HasHtml;
import com.webforj.concern.HasJsExecution;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.exceptions.WebforjRuntimeException;
import com.webforj.utilities.BBjFunctionalityHelper;
import java.util.ArrayList;
import java.util.Base64;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * An HTML Element component.
 *
 * <p>
 * This class represents an HTML Element. It provides methods to set and get HTML content,
 * manipulate properties, and add event listeners to the element.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
public final class Element extends DwcContainer<Element>
    implements HasHtml<Element>, HasFocus<Element>, HasEnablement<Element>, HasJsExecution {

  private final String nodeName;
  private final Map<String, EventSinkListenerRegistry<ElementEvent>> registries = new HashMap<>();
  private final Map<String, Object> properties = new HashMap<>();
  private final DwcFocusableMixin<Element> focusableMixin = new DwcFocusableMixin<>(this);
  private final JsExecutor jsExecutor = new ElementJsExecutor(this);
  private final EventSinkListenerRegistry<ElementDefinedEvent> definedEventSinkListenerRegistry =
      new EventSinkListenerRegistry<>(new ElementDefinedEventSink(this, getEventDispatcher()),
          ElementDefinedEvent.class);
  private final EventSinkListenerRegistry<ExecuteAsyncScriptEvent> scriptEventSinkListenerRegistry =
      new EventSinkListenerRegistry<>(new ExecuteAsyncScriptEventSink(this, getEventDispatcher()),
          ExecuteAsyncScriptEvent.class);
  private final List<PendingResult<Element>> whenDefinedResults = new ArrayList<>();
  private final Map<String, List<Component>> slots = new HashMap<>();
  private boolean isDefined = false;
  private String html;

  /**
   * Constructs an HTML Element with the given tag name.
   *
   * @param node the node name
   * @param html the HTML content
   *
   * @throws NullPointerException if the given node name is null
   */
  public Element(String node, String html) {
    this(node);
    setHtml(html);
  }

  /**
   * Constructs an HTML Element with the given tag name.
   *
   * @param node the node name
   *
   * @throws NullPointerException if the given node name is null
   */
  public Element(String node) {
    super();
    if (node == null || node.trim().isEmpty()) {
      throw new NullPointerException("The HTML node name cannot be null or empty");
    }

    this.nodeName = node;
    scriptEventSinkListenerRegistry.addEventListener(this::onExecuteAsyncScriptEvent);
    definedEventSinkListenerRegistry.addEventListener(this::onDefinedEventEvent);
  }

  /**
   * Constructs an HTML div Element.
   */
  public Element() {
    this("div");
  }

  /**
   * Get the tag name associated with this element.
   *
   * @return the tagName
   */
  public String getNodeName() {
    return nodeName;
  }

  /**
   * Check whether the element is defined or not.
   *
   * @return true when the named element is defined, false otherwise
   */
  public boolean isDefined() {
    return isDefined;
  }

  /**
   * Returns a {@link PendingResult} that completes when the named element is defined.
   *
   * @return A {@link PendingResult} that completes when a element becomes defined with the given
   *         name. If a element has already been defined with the name, the PendingResult will
   *         immediately complete.
   */
  public PendingResult<Element> whenDefined() {
    if (isDefined()) {
      return PendingResult.completedWith(this);
    }

    PendingResult<Element> result = new PendingResult<>();
    whenDefinedResults.add(result);
    return result;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Element setHtml(String html) {
    BBjWebComponent control = inferControl();

    if (control != null) {
      try {
        control.setHtml(html);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    this.html = html;
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getHtml() {
    BBjWebComponent control = inferControl();

    if (control != null) {
      try {
        return control.getHtml();
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    return html;
  }

  /**
   * Adds a {@link ElementEvent} listener for the component.
   *
   * @param type the type/name of the event. (e.g. "click").
   * @param listener the event listener to be added
   * @param options the options associated with the event listener
   * @param processEventOptionsAnnotation whether to process the {@link EventOptions} annotation of
   *        the event listener or not.
   *
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<ElementEvent> addEventListener(String type,
      EventListener<ElementEvent> listener, ElementEventOptions options,
      boolean processEventOptionsAnnotation) {

    ElementEventOptions optionsFromListener = null;

    if (processEventOptionsAnnotation) {
      optionsFromListener = ElementAnnotationProcessor.processEventOptions(listener.getClass());
    }

    final ElementEventOptions finalEventOptions =
        (new ElementEventOptions()).mergeWith(optionsFromListener, options);

    // create a sink for each event type
    EventSinkListenerRegistry<ElementEvent> registry = registries.computeIfAbsent(type, k -> {
      return new EventSinkListenerRegistry<>(new ElementEventSink(this, type, getEventDispatcher()),
          ElementEvent.class);
    });

    return registry.addEventListener(new EventListener<ElementEvent>() {
      @Override
      public void onEvent(ElementEvent event) {
        if (String.valueOf(event.getId()).equals(registry.getCallbackId(this))) {
          listener.onEvent(event);
        }
      }
    }, finalEventOptions);
  }

  /**
   * Adds a {@link ElementEvent} listener for the component.
   *
   * @param type the type/name of the event. (e.g. "click").
   * @param listener the event listener to be added
   * @param options the options associated with the event listener
   *
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<ElementEvent> addEventListener(String type,
      EventListener<ElementEvent> listener, ElementEventOptions options) {
    return addEventListener(type, listener, options, true);
  }

  /**
   * Adds a {@link ElementEvent} listener for the component.
   *
   * @param type the type/name of the event. (e.g. "click").
   * @param listener the event listener to be added
   *
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<ElementEvent> addEventListener(String type,
      EventListener<ElementEvent> listener) {
    return addEventListener(type, listener, null);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public Object executeJs(String script) {
    return jsExecutor.executeJs(script);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public PendingResult<Object> executeJsAsync(String script) {
    return jsExecutor.executeJsAsync(script);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void executeJsVoidAsync(String js) {
    jsExecutor.executeJsVoidAsync(js);
  }

  /**
   * Synchronously calls a specified JavaScript function on an HTML element with provided arguments.
   *
   * <p>
   * This method is used to invoke a JavaScript function named {@code functionName} on the context
   * of an HTML element. The invocation occurs only after the element is part of the Document Object
   * Model (DOM). If the element is not in the DOM, the function returns {@code null} and logs a
   * warning in the browser console.
   * </p>
   *
   * <p>
   * Arguments passed to the function are serialized as a JSON array. Special argument types are
   * handled as follows:
   * </p>
   * <ul>
   * <li>{@code this} is replaced with the current instance of the client element.</li>
   * <li>Any {@code Component} instance is replaced with its corresponding client component
   * instance, provided it is attached to the DOM.</li>
   * </ul>
   *
   * <p>
   * This synchronous invocation blocks the executing thread until completion. It uses the
   * {@link #executeJs(String)} method for JavaScript execution. This differs from the asynchronous
   * {@link #executeJsAsync(String)}, as it does not wait for {@code Component} arguments to attach
   * to the DOM, which can result in failure if the components are not yet attached.
   * </p>
   *
   * <p>
   * <b>Example Usage:</b>
   *
   * <pre>
   * {@code
   * // Assuming 'component' is a valid component attached to the DOM
   * Object result = el.callJsFunction("functionName", component, "arg1", 123);
   * }
   * </pre>
   * </p>
   *
   * @param functionName the name of the JavaScript function to call
   * @param arguments the arguments to pass to the function
   *
   * @return the result of the function call, or {@code null} if the function does not exist or the
   *         element is not attached to the DOM
   *
   * @see #callJsFunctionAsync(String, Object...)
   * @see #executeJs(String)
   */
  public Object callJsFunction(String functionName, Object... arguments) {
    String script = buildCallJsFunctionScript(functionName, true, arguments);
    return executeJs(script);
  }

  /**
   * Asynchronously calls a specified JavaScript function on an HTML element with provided
   * arguments.
   *
   * <p>
   * This method is similar to {@link #callJsFunction(String, Object...)} but operates
   * asynchronously. It invokes the JavaScript function named {@code functionName} on the context of
   * an HTML element. The function execution is queued until the element is attached to the DOM.
   * </p>
   *
   * <p>
   * Arguments are serialized as a JSON array. Special argument types are handled as follows:
   * </p>
   * <ul>
   * <li>{@code this} is replaced with the current instance of the client element.</li>
   * <li>Any {@code Component} instance is replaced with its corresponding client component
   * instance, once it is attached to the DOM.</li>
   * </ul>
   *
   * <p>
   * This method does not block the executing thread. It uses {@link #executeJsAsync(String)} for
   * JavaScript execution. The method waits for all {@code Component} arguments to attach to the
   * DOM, which means the invocation may never complete if a component is never attached.
   * </p>
   *
   * <p>
   * <b>Example Usage:</b>
   *
   * <pre>
   * {@code
   * // Assuming 'component' is a valid component that may or may not be attached to the DOM yet
   * PendingResult<Object> result = el.callJsFunctionAsync("functionName", component, "arg1", 123);
   * result.thenAccept(System.out::println);
   * }
   * </pre>
   * </p>
   *
   * @param functionName the name of the JavaScript function to call
   * @param arguments the arguments to pass to the function
   *
   * @return a {@link PendingResult} representing the asynchronous result of the function call
   *
   * @see #callJsFunction(String, Object...)
   * @see #executeJsAsync(String)
   * @see PendingResult
   */
  public PendingResult<Object> callJsFunctionAsync(String functionName, Object... arguments) {
    PendingResult<Object> result = new PendingResult<>();

    waitForAllComponents(arguments).thenAccept(untilAttached -> { // NOSONAR
      whenDefined().thenAccept(element -> {
        String script = buildCallJsFunctionScript(functionName, true, arguments);
        executeJsAsync(script).thenAccept(result::complete);
      });
    });

    return result;
  }

  /**
   * Asynchronously calls a specified JavaScript function on an HTML element with provided arguments
   * without returning a result to the server.
   *
   * <p>
   * This method is similar to {@link #callJsFunction(String, Object...)} but operates
   * asynchronously. It invokes the JavaScript function named {@code functionName} on the context of
   * an HTML element. The function execution is queued until the element is attached to the DOM.
   * </p>
   *
   * <p>
   * Arguments are serialized as a JSON array. Special argument types are handled as follows:
   * </p>
   * <ul>
   * <li>{@code this} is replaced with the current instance of the client element.</li>
   * <li>Any {@code Component} instance is replaced with its corresponding client component
   * instance, once it is attached to the DOM.</li>
   * </ul>
   *
   * <p>
   * This method does not block the executing thread. It uses {@link #executeJsAsync(String)} for
   * JavaScript execution. The method waits for all {@code Component} arguments to attach to the
   * DOM, which means the invocation may never complete if a component is never attached.
   * </p>
   *
   * <p>
   * <b>Example Usage:</b>
   *
   * <pre>
   * {@code
   * // Assuming 'component' is a valid component that may or may not be attached to the DOM yet
   * el.callJsFunctionVoidAsync("functionName", component, "arg1", 123);
   * }
   * </pre>
   * </p>
   *
   * @param functionName the name of the JavaScript function to call
   * @param arguments the arguments to pass to the function
   *
   * @see #callJsFunctionAsync(String, Object...)
   * @see #executeJsVoidAsync(String)
   * @since 24.11
   */
  public void callJsFunctionVoidAsync(String functionName, Object... arguments) {
    waitForAllComponents(arguments).thenAccept(untilAttached -> { // NOSONAR
      whenDefined().thenAccept(element -> {
        String script = buildCallJsFunctionScript(functionName, false, arguments);
        executeJsVoidAsync(script);
      });
    });
  }

  /**
   * Adds the given components as children of this component and associates them with the given slot
   * name.
   *
   * @param slot the slot name
   * @param component the components to add.
   *
   * @throws NullPointerException if the given components is null.
   * @throws IllegalArgumentException if the given components cannot be added by this component.
   * @throws IllegalStateException if the given components is destroyed.
   */
  public void add(String slot, Component... component) {
    slots.computeIfAbsent(slot, k -> new ArrayList<>()).addAll(List.of(component));
    add(component);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void remove(Component... components) {
    super.remove(components);

    // remove the components from the slots
    for (Component component : components) {
      String slot = findComponentSlot(component);
      if (!slot.isEmpty()) {
        slots.get(slot).remove(component);
      }
    }
  }

  /**
   * Searches for the given component across all slots and returns the name of the slot where this
   * component is located. If the component is not found in any slot, an empty {@code String} is
   * returned.
   *
   * @param component The {@link Component} to be searched for in the slots.
   * @return The name of the slot where the component is located, or an empty {@code String} if the
   *         component is not found in any slot.
   */
  public String findComponentSlot(Component component) {
    for (Map.Entry<String, List<Component>> entry : slots.entrySet()) {
      if (entry.getValue().contains(component)) {
        // Return the slot name (key) where the component is found
        return entry.getKey();
      }
    }

    return "";
  }

  /**
   * Returns the list of components assigned to the given slot.
   *
   * @param slot the slot name
   * @return the list of components assigned to the given slot
   */
  public List<Component> getComponentsInSlot(String slot) {
    return slots.getOrDefault(slot, Collections.emptyList());
  }

  /**
   * Returns the list of components of a specific type assigned to the given slot.
   *
   * @param slot the slot name
   * @param classOfT the class of the type T
   *
   * @return the list of components of type T assigned to the given slot
   */
  public <T extends Component> List<T> getComponentsInSlot(String slot, Class<T> classOfT) {
    return slots.getOrDefault(slot, Collections.emptyList()).stream().filter(classOfT::isInstance)
        .map(classOfT::cast).toList();
  }

  /**
   * Returns the first component assigned to the given slot.
   *
   * @param slot the slot name
   * @return the first component assigned to the given slot
   */
  public Component getFirstComponentInSlot(String slot) {
    return getComponentsInSlot(slot).stream().findFirst().orElse(null);
  }

  /**
   * Returns the first component of a specific type assigned to the given slot.
   *
   * @param slot the slot name
   * @param classOfT the class of the type T
   *
   * @return the first component of type T assigned to the given slot, or null if none is found
   */
  public <T extends Component> T getFirstComponentInSlot(String slot, Class<T> classOfT) {
    return getComponentsInSlot(slot, classOfT).stream().findFirst().orElse(null);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public Element setEnabled(boolean enabled) {
    return focusableMixin.setEnabled(enabled);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public boolean isEnabled() {
    return focusableMixin.isEnabled();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public Element setFocusable(boolean focusable) {
    return focusableMixin.setFocusable(focusable);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public boolean isFocusable() {
    return focusableMixin.isFocusable();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public Element focus() {
    return focusableMixin.focus();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public ListenerRegistration<FocusEvent> addFocusListener(EventListener<FocusEvent> listener) {
    return focusableMixin.addFocusListener(listener);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public ListenerRegistration<BlurEvent> addBlurListener(EventListener<BlurEvent> listener) {
    return focusableMixin.addBlurListener(listener);
  }

  /**
   * Handles the {@link ExecuteAsyncScriptEvent}.
   *
   * @param event the execution event
   */
  void onExecuteAsyncScriptEvent(ExecuteAsyncScriptEvent event) {
    int index = event.getIndex();
    Object result = event.getResult();
    PendingResult<Object> pending = jsExecutor.getPendingResultByIndex(index);

    if (pending != null) {
      pending.complete(result);
    }
  }

  /**
   * Handles the {@link ElementDefinedEvent}.
   *
   * @param event the defined event
   */
  void onDefinedEventEvent(ElementDefinedEvent event) {
    isDefined = true;
    whenDefinedResults.forEach(r -> r.complete(this));
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected final boolean allowMultipleAttach() {
    return true;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onCreate(Window window) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(window);
      byte[] flags =
          BBjFunctionalityHelper.buildStandardCreationFlags(this.isVisible(), this.isEnabled());
      setControl(w.addWebComponent(getNodeName(), flags));
    } catch (Exception e) {
      throw new WebforjRuntimeException("Failed to create the BBjWebComponent Control", e);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void attachControlCallbacks() {
    super.attachControlCallbacks();
    focusableMixin.attachControlCallbacks();
    scriptEventSinkListenerRegistry.attach();
    definedEventSinkListenerRegistry.attach();

    // loop through all the sinks and attach the callbacks
    registries.values().forEach(sink -> sink.attach());
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onAttach() {
    if (html != null && !html.isEmpty()) {
      setHtml(html);
    }

    super.onAttach();
    focusableMixin.onAttach();

    if (!properties.isEmpty()) {
      for (Map.Entry<String, Object> entry : properties.entrySet()) {
        setProperty(entry.getKey(), entry.getValue());
      }
    }

    jsExecutor.executeQueuedScripts();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void doAdd(Component component) {
    try {
      // create the component
      ComponentAccessor.getDefault().create(component, getWindow());

      // add the component to the slot
      if (!(component instanceof RadioButtonGroup)) {
        String slot = findComponentSlot(component);
        BBjControl control = ComponentAccessor.getDefault().getControl(component);
        inferControl().setSlot(slot, control);
      }
    } catch (IllegalAccessException | BBjException e) {
      throw new IllegalArgumentException(
          "Attempted to add an incompatible component of type '" + component.getClass().getName()
              + "'. " + "Components must be a subclass of DwcComponent or extend Composite. "
              + "Check if the component is correctly extending or implementing these " + "classes.",
          e);
    }
  }

  /**
   * Find the control associated with this component.
   *
   * @return the control associated with this component
   */
  private BBjWebComponent inferControl() {
    try {
      return (BBjWebComponent) ComponentAccessor.getDefault().getControl(this);
    } catch (IllegalAccessException e) {
      throw new WebforjRuntimeException(e);
    }
  }

  /**
   * Waits for all passed {@code Component} instances to be attached to the DOM.
   *
   * <p>
   * This helper method is used internally to ensure that all components passed as arguments to
   * asynchronous JavaScript function calls are attached to the DOM before the function is executed.
   * It returns a {@link PendingResult} which completes once all components are attached.
   * </p>
   *
   * <p>
   * If no {@code Component} instances are passed, the method immediately returns a completed
   * {@link PendingResult}.
   * </p>
   *
   * @param arguments the arguments among which to find {@code Component} instances
   * @return a {@link PendingResult} that completes when all {@code Component} instances are
   *         attached to the DOM
   */
  private PendingResult<Void> waitForAllComponents(Object... arguments) {
    List<Object> components =
        List.of(arguments).stream().filter(Component.class::isInstance).toList();

    if (!components.isEmpty()) {
      List<PendingResult<Component>> pendingResults = new ArrayList<>();

      for (Object component : components) {
        pendingResults.add(((Component) component).whenAttached());
      }

      return PendingResult.allOf(pendingResults.toArray(new PendingResult[0]));
    }

    return PendingResult.completedWith(null);
  }

  /**
   * Builds a JavaScript script for invoking a function on an HTML element, considering various
   * conditions and serialization of arguments.
   *
   * <p>
   * This method constructs a JavaScript script that calls a specified function with the given name
   * on an HTML element, considering the following conditions:
   * </p>
   *
   * <ul>
   * <li>Serializes the function arguments as a JSON array to be passed to the JavaScript
   * function.</li>
   * <li>Waiting for custom elements to be defined.</li>
   * <li>Logs a warning message and returns {@code null} if the element is not attached to the DOM
   * or if the specified function does not exist on the element</li>
   * <li>Deserializes the arguments on the client side and calls the JavaScript function on the
   * element.</li>
   * </ul>
   *
   * <p>
   * The generated script is wrapped within an asynchronous function.
   * </p>
   *
   * @param functionName The name of the JavaScript function to be called.
   * @param shouldWait Whether to wait for the result of the function call or not.
   * @param arguments The arguments to be passed to the JavaScript function.
   *
   * @return A JavaScript script that, when executed, will perform the specified function call with
   *         the given arguments on the HTML element. If any condition is not met, the script may
   *         return {@code null}.
   */
  String buildCallJsFunctionScript(String functionName, boolean shouldWait, Object... arguments) {
    if (functionName == null || functionName.trim().isEmpty()) {
      throw new IllegalArgumentException("The function name cannot be null or empty");
    }

    if (functionName.startsWith(".")) {
      throw new IllegalArgumentException("The function name cannot start with a dot (.) character");
    }

    StringBuilder sb = new StringBuilder();
    sb.append("(async () => {");

    // Serialize arguments as a JSON array
    Gson gson = new Gson();
    JsonArray jsonArgsArray = new JsonArray();
    for (Object arg : arguments) {
      if (arg instanceof Component) {
        // For Component instances, get the client component ID
        String clientComponentId = ((Component) arg).getClientComponentId();
        jsonArgsArray.add("objects.get('" + clientComponentId + "')");
      } else {
        // For other arguments, serialize them
        JsonElement jsonElement = gson.toJsonTree(arg);
        jsonArgsArray.add(jsonElement);
      }
    }

    // Convert the JsonArray to a String representation
    String jsonArgs = gson.toJson(jsonArgsArray);
    jsonArgs = jsonArgs.replace("'", "\\'");

    String node = getNodeName();

    // Check if the element name contains a hyphen, indicating a custom element
    sb.append("const isCustomElement = '").append(node).append("'.includes('-');");

    // Wait for the element definition if it's a custom element
    sb.append("if (isCustomElement) {");
    sb.append("  await customElements.whenDefined('").append(node).append("');");
    sb.append("}");

    // Check if component is null
    sb.append("if (!component) {");
    sb.append("  console.warn('The element \"").append(node)
        .append("\" is not attached to the DOM');");
    sb.append("  return null;");
    sb.append("}");

    // Check if the function exists on the component
    sb.append("if (typeof component['").append(functionName).append("'] !== 'function') {");
    sb.append("  console.warn('The function \"").append(functionName)
        .append("\" does not exist on the element \"").append(node).append("\"');");
    sb.append("  return null;");
    sb.append("}");

    // Deserialize arguments on the client side and call the function
    sb.append("const rawArgs = JSON.parse(atob(`")
        .append(Base64.getEncoder().encodeToString(jsonArgs.getBytes())).append("`));");

    // Resolve the special arguments
    sb.append("const args = rawArgs.map(arg => {");
    sb.append("  if (arg === 'this') {");
    sb.append("    return component;");
    sb.append("  }");
    sb.append("  if (typeof arg === 'string' && arg.startsWith('objects.get')) {");
    sb.append("    return eval(arg);");
    sb.append("  }");
    sb.append("  return arg;");
    sb.append("});");

    if (shouldWait) {
      sb.append("return await component['").append(functionName)
          .append("'].apply(component, args);");
    } else {
      sb.append("component['").append(functionName).append("'].apply(component, args);");
    }

    sb.append("})();"); // end of async function

    return sb.toString();
  }

  /**
   * A JsExecutor implementation for the Element component.
   */
  private final class ElementJsExecutor extends JsExecutor {
    private ElementJsExecutor(Component component) {
      super(component);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected Object doExecuteJs(String script) throws BBjException {
      return inferControl().executeScript(script, true);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected int doExecuteJsAsync(String script) throws BBjException {
      return inferControl().executeAsyncScript(script, true, true);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected int doExecuteJsVoidAsync(String script) throws BBjException {
      return inferControl().executeAsyncScript(script, true, false);
    }
  }
}
