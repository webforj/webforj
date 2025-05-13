package com.webforj.component.tree.event;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.event.BBjTreeNodeExpandedEvent;
import com.webforj.component.tree.Tree;
import com.webforj.component.tree.TreeNode;
import com.webforj.component.tree.sink.TreeExpandEventSink;
import com.webforj.dispatcher.EventDispatcher;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

class TreeExpandEventTest {

  @Test
  void payload() {
    BBjTreeNodeExpandedEvent eventMock = Mockito.mock(BBjTreeNodeExpandedEvent.class);
    EventDispatcher dispatcher = new EventDispatcher();

    // Capture the dispatched event
    TreeExpandEvent[] dispatchedEvent = new TreeExpandEvent[1];

    // Add event listener
    dispatcher.addListener(TreeExpandEvent.class, e -> {
      dispatchedEvent[0] = e;
    });

    Tree tree = new Tree();
    TreeNode node1 = tree.add("Node 1");
    when(eventMock.getNodeID()).thenReturn(node1.getUniqueId());

    TreeExpandEventSink sink = new TreeExpandEventSink(tree, dispatcher);
    // Invoke the event handler
    sink.handleEvent(eventMock);

    assertEquals(node1, dispatchedEvent[0].getNode());
    tree.destroy();
  }
}
