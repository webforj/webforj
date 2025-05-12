package com.webforj.component.tree.event;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.event.BBjTreeMouseDoubleClickEvent;
import com.webforj.component.tree.Tree;
import com.webforj.component.tree.TreeNode;
import com.webforj.component.tree.sink.TreeDoubleClickEventSink;
import com.webforj.dispatcher.EventDispatcher;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

class TreeMouseEventTest {

  @Test
  void payload() {
    BBjTreeMouseDoubleClickEvent eventMock = Mockito.mock(BBjTreeMouseDoubleClickEvent.class);
    EventDispatcher dispatcher = new EventDispatcher();

    // Capture the dispatched event
    TreeDoubleClickEvent[] dispatchedEvent = new TreeDoubleClickEvent[1];

    // Add event listener
    dispatcher.addListener(TreeDoubleClickEvent.class, e -> {
      dispatchedEvent[0] = e;
    });

    Tree tree = new Tree();
    TreeNode node1 = tree.add("Node 1");

    when(eventMock.getExactNodeID()).thenReturn(node1.getUniqueId());
    when(eventMock.getX()).thenReturn(10);
    when(eventMock.getY()).thenReturn(20);
    when(eventMock.getMouseButton()).thenReturn(1);
    when(eventMock.isAltDown()).thenReturn(true);
    when(eventMock.isCmdDown()).thenReturn(false);
    when(eventMock.isControlDown()).thenReturn(true);
    when(eventMock.isShiftDown()).thenReturn(false);


    TreeDoubleClickEventSink sink = new TreeDoubleClickEventSink(tree, dispatcher);
    // Invoke the event handler
    sink.handleEvent(eventMock);

    assertEquals(node1, dispatchedEvent[0].getNode());
    assertEquals(10, dispatchedEvent[0].getX());
    assertEquals(20, dispatchedEvent[0].getY());
    assertEquals(1, dispatchedEvent[0].getMouseButton());
    assertEquals(true, dispatchedEvent[0].isAltDown());
    assertEquals(false, dispatchedEvent[0].isCmdDown());
    assertEquals(true, dispatchedEvent[0].isControlDown());
    assertEquals(false, dispatchedEvent[0].isShiftDown());

    tree.destroy();
  }
}
