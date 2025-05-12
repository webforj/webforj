package com.webforj.component.tree.event;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.event.BBjTreeNodeDeselectedEvent;
import com.webforj.component.tree.Tree;
import com.webforj.component.tree.TreeNode;
import com.webforj.component.tree.sink.TreeDeselectEventSink;
import com.webforj.dispatcher.EventDispatcher;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

class TreeDeselectEventTest {

  @Test
  void payload() {
    BBjTreeNodeDeselectedEvent eventMock = Mockito.mock(BBjTreeNodeDeselectedEvent.class);
    EventDispatcher dispatcher = new EventDispatcher();

    // Capture the dispatched event
    TreeDeselectEvent[] dispatchedEvent = new TreeDeselectEvent[1];

    when(eventMock.getNodeID()).thenReturn(1);

    // Add event listener
    dispatcher.addListener(TreeDeselectEvent.class, e -> {
      dispatchedEvent[0] = e;
    });

    Tree tree = new Tree();
    TreeNode node1 = tree.add("Node 1");

    TreeDeselectEventSink sink = new TreeDeselectEventSink(tree, dispatcher);
    // Invoke the event handler
    sink.handleEvent(eventMock);

    assertEquals(node1, dispatchedEvent[0].getNode());
    tree.destroy();
  }
}
