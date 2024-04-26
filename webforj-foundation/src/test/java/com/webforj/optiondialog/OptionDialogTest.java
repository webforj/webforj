package com.webforj.optiondialog;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import com.webforj.optiondialog.MessageDialog.IconType;
import com.webforj.optiondialog.MessageDialog.MessageType;
import org.junit.jupiter.api.Test;

class OptionDialogTest {

  @Test
  void shouldCreateMessageDialogWithAllParameters() {
    MessageDialog dialog = OptionDialog.createMessageDialog("Title", "Message", MessageType.YES_NO,
        IconType.INFORMATION);
    assertNotNull(dialog);
    assertEquals("Title", dialog.getTitle());
    assertEquals("Message", dialog.getMessage());
    assertEquals(MessageType.YES_NO, dialog.getMessageType());
    assertEquals(IconType.INFORMATION, dialog.getIconType());
  }

  @Test
  void shouldCreateMessageDialogWithMessageTypeOnly() {
    MessageDialog dialog =
        OptionDialog.createMessageDialog("Title", "Message", MessageType.ABORT_RETRY_IGNORE);
    assertNotNull(dialog);
    assertEquals(MessageType.ABORT_RETRY_IGNORE, dialog.getMessageType());
    assertEquals(IconType.NONE, dialog.getIconType());
  }

  @Test
  void shouldCreateMessageDialogWithDefaults() {
    MessageDialog dialog = OptionDialog.createMessageDialog("Title", "Message");
    assertNotNull(dialog);
    assertEquals("Title", dialog.getTitle());
    assertEquals("Message", dialog.getMessage());
    assertEquals(MessageType.OK, dialog.getMessageType());
    assertEquals(IconType.NONE, dialog.getIconType());
  }

  @Test
  void shouldCreateMessageDialogWithMessageOnly() {
    MessageDialog dialog = OptionDialog.createMessageDialog("Message");
    assertNotNull(dialog);
    assertEquals("Message", dialog.getMessage());
    assertEquals(MessageType.OK, dialog.getMessageType());
    assertEquals(IconType.NONE, dialog.getIconType());
  }
}
