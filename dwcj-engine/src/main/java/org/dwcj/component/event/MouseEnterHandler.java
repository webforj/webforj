// package org.dwcj.component.event;

// import org.dwcj.App;
// import org.dwcj.component.AbstractDwcComponent;
// import org.dwcj.component.event.sink.MouseEnterEventSink;

// /**
// * Handles the implementation of the MouseEnterEvent.
// */
// public class MouseEnterHandler implements HasMouseEnter {

// private final EventDispatcher dispatcher;
// private final MouseEnterEventSink sink;

// /**
// * Contructor, creates a MouseEnterEventSink.
// *
// * @param component the component which uses this handler
// * @param dispatcher the dispatcher of the component
// */
// public MouseEnterHandler(AbstractDwcComponent component, EventDispatcher dispatcher) {
// this.dispatcher = dispatcher;
// this.sink = new MouseEnterEventSink(component, dispatcher);
// }

// // /**
// // * Creates the Sink.
// // */
// // public void create() {
// // this.sink = new MouseEnterEventSink(component, dispatcher);
// // }

// @Override
// public T addMouseEnterListener(EventListener<MouseEnterEvent> listener) {
// // if (this.sink != null && this.dispatcher.getListenersCount(MouseEnterEvent.class) > 0) {
// // }
// this.dispatcher.addEventListener(MouseEnterEvent.class, listener);
// this.sink.setCallback();
// App.consoleLog("added MousEnterListener");
// return component;
// }

// @Override
// public T onMouseEnter(EventListener<MouseEnterEvent> listener) {
// return addMouseEnterListener(listener);
// }

// @Override
// public T removeMouseEnterListener(EventListener<MouseEnterEvent> listener) {
// this.dispatcher.removeEventListener(MouseEnterEvent.class, listener);
// if (this.sink != null && this.dispatcher.getListenersCount(MouseEnterEvent.class) == 0) {
// this.sink.removeCallback();
// }
// return component;
// }

// /**
// * Sets the callback for the sink if the dispatcher holds any MouseEnterEvents.
// */
// public void catchUp() {
// App.consoleLog("called catchUp in Handler");
// if (this.dispatcher.getListenersCount(MouseEnterEvent.class) > 0) {
// App.consoleLog("setCallbakc on Sink");
// this.sink.setCallback();
// }
// }

// }
