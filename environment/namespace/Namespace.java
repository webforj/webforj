public abstract class Namespace {

  private EventDispatcher createEventDispatcher(String key) {
    return variableDispatchers.computeIfAbsent(key, k -> new EventDispatcher());
  }

  public ListenerRegistration<NamespaceVariableAccessEvent> addVariableAccessListener(String key,
      EventListener<NamespaceVariableAccessEvent> listener) {
    EventDispatcher dispatcher = createEventDispatcher(key);
    return dispatcher.addListener(NamespaceVariableAccessEvent.class, listener);
  }

  public ListenerRegistration<NamespaceVariableChangeEvent> addVariableChangeListener(String key,
      EventListener<NamespaceVariableChangeEvent> listener) {
    EventDispatcher dispatcher = createEventDispatcher(key);
    return dispatcher.addListener(NamespaceVariableChangeEvent.class, listener);
  }

  final void handleNamespaceVariableAccessEvent(BBjNamespaceEvent ev) {
    EventDispatcher dispatcher = createEventDispatcher(variable);
  }

  final void handleNamespaceVariableChangeEvent(BBjNamespaceEvent ev) {
    EventDispatcher dispatcher = createEventDispatcher(variable);
  }
}
