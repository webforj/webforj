package com.webforj.component.tree.event;


import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.event.BBjTreeNodeSelectedEvent;
import com.webforj.component.tree.Tree;
import com.webforj.component.tree.TreeNode;
import com.webforj.component.tree.sink.TreeSelectEventSink;
import com.webforj.dispatcher.EventDispatcher;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

class TreeSelectEventTest {

  @Test
  void payload() {
    BBjTreeNodeSelectedEvent eventMock = Mockito.mock(BBjTreeNodeSelectedEvent.class);
    EventDispatcher dispatcher = new EventDispatcher();

    // Capture the dispatched event
    TreeSelectEvent[] dispatchedEvent = new TreeSelectEvent[1];

    when(eventMock.getNodeID()).thenReturn(1);

    // Add event listener
    dispatcher.addListener(TreeSelectEvent.class, e -> {
      dispatchedEvent[0] = e;
    });

    Tree tree = new Tree();
    TreeNode node1 = tree.add("Node 1");

    TreeSelectEventSink sink = new TreeSelectEventSink(tree, dispatcher);
    // Invoke the event handler
    sink.handleEvent(eventMock);

    assertEquals(node1, dispatchedEvent[0].getNode());
    tree.destroy();
  }
}
