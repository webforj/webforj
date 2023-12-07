package org.dwcj;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;

class MessageActionTest {

  @Test
  void testConstructorAndGetMessage() {
    String message = "Test Message";
    MessageAction action = new MessageAction(message);

    assertNotNull(action);
    assertEquals(message, action.getMessage());
  }

  @Test
  void testGetMessage() {
    String message = "Another Message";
    MessageAction action = new MessageAction(message);
    String retrievedMessage = action.getMessage();

    assertEquals(message, retrievedMessage);
  }
}
