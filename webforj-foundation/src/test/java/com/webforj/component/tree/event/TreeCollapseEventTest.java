package com.webforj.component.tree.event;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.event.BBjTreeNodeCollapsedEvent;
import com.webforj.component.tree.Tree;
import com.webforj.component.tree.TreeNode;
import com.webforj.component.tree.sink.TreeCollapseEventSink;
import com.webforj.dispatcher.EventDispatcher;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

class TreeCollapseEventTest {

  @Test
  void payload() {
    BBjTreeNodeCollapsedEvent eventMock = Mockito.mock(BBjTreeNodeCollapsedEvent.class);
    EventDispatcher dispatcher = new EventDispatcher();

    // Capture the dispatched event
    TreeCollapseEvent[] dispatchedEvent = new TreeCollapseEvent[1];

    when(eventMock.getNodeID()).thenReturn(1);

    // Add event listener
    dispatcher.addListener(TreeCollapseEvent.class, e -> {
      dispatchedEvent[0] = e;
    });

    Tree tree = new Tree();
    TreeNode node1 = tree.add("Node 1");

    TreeCollapseEventSink sink = new TreeCollapseEventSink(tree, dispatcher);
    // Invoke the event handler
    sink.handleEvent(eventMock);

    assertEquals(node1, dispatchedEvent[0].getNode());
    tree.destroy();
  }
}
