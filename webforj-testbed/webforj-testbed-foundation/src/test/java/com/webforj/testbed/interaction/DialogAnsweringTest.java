package com.webforj.testbed.interaction;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.UploadedFile;
import com.webforj.component.optiondialog.ConfirmDialog;
import com.webforj.component.optiondialog.OptionDialog;
import com.webforj.testbed.ShownOptionDialog;
import com.webforj.testbed.TestbedTestSupport;
import com.webforj.testbed.WebforjTestbed;
import java.util.List;
import org.junit.jupiter.api.Test;

class DialogAnsweringTest extends TestbedTestSupport {
  private static final String ORDER_QUESTION = "Delete the order?";
  private static final String ORDERS_TITLE = "Orders";
  private static final String SAVED_MESSAGE = "Saved";
  private static final String WELCOME_TITLE = "Welcome";
  private static final String NAME_QUESTION = "Your name?";

  @Test
  void shouldAnswerConfirmDialogsInOrder() {
    WebforjTestbed.getCurrent().answerConfirmDialog(ConfirmDialog.Result.YES);
    WebforjTestbed.getCurrent().answerConfirmDialog(ConfirmDialog.Result.NO);

    ConfirmDialog.Result first = OptionDialog.showConfirmDialog(ORDER_QUESTION, ORDERS_TITLE,
        ConfirmDialog.OptionType.YES_NO);
    ConfirmDialog.Result second = OptionDialog.showConfirmDialog("Delete the invoice?", "Invoices",
        ConfirmDialog.OptionType.YES_NO);

    assertEquals(List.of(ConfirmDialog.Result.YES, ConfirmDialog.Result.NO),
        List.of(first, second));
  }

  @Test
  void shouldRecordTheDialogsShown() {
    WebforjTestbed.getCurrent().answerConfirmDialog(ConfirmDialog.Result.OK);
    OptionDialog.showConfirmDialog(ORDER_QUESTION, ORDERS_TITLE);
    OptionDialog.showMessageDialog(SAVED_MESSAGE, ORDERS_TITLE);

    List<ShownOptionDialog> shown = WebforjTestbed.getCurrent().getShownOptionDialogs();

    assertEquals(2, shown.size());
    assertEquals(ShownOptionDialog.Kind.CONFIRM, shown.get(0).getKind());
    assertEquals(ORDERS_TITLE, shown.get(0).getTitle());
    assertEquals(ORDER_QUESTION, shown.get(0).getMessage());
    assertEquals(SAVED_MESSAGE, shown.get(1).getMessage());
  }

  @Test
  void shouldDismissMessageDialogsWithoutAnAnswer() {
    WebforjTestbed context = WebforjTestbed.getCurrent();
    OptionDialog.showMessageDialog(SAVED_MESSAGE);

    assertEquals(1, context.getShownOptionDialogs().size());
  }

  @Test
  void shouldFailWhenTheConfirmDialogHasNoAnswer() {
    IllegalStateException failure = assertThrows(IllegalStateException.class, () -> OptionDialog
        .showConfirmDialog(ORDER_QUESTION, ORDERS_TITLE, ConfirmDialog.OptionType.YES_NO));

    assertTrue(failure.getMessage().contains("titled 'Orders'"));
    assertTrue(failure.getMessage().contains("WebforjTestbed.getCurrent().answerConfirmDialog"));
  }

  @Test
  void shouldAnswerAndCancelInputDialogs() {
    WebforjTestbed.getCurrent().answerInputDialog("Ada");
    WebforjTestbed.getCurrent().answerInputDialog(null);

    assertEquals("Ada", OptionDialog.showInputDialog(NAME_QUESTION, WELCOME_TITLE));
    assertNull(OptionDialog.showInputDialog(NAME_QUESTION, WELCOME_TITLE));
    assertThrows(IllegalStateException.class,
        () -> OptionDialog.showInputDialog(NAME_QUESTION, WELCOME_TITLE));
  }

  @Test
  void shouldAnswerFileDialogsByKind() {
    WebforjTestbed.getCurrent().answerFileChooserDialog("/data/report.csv");
    WebforjTestbed.getCurrent().answerFileSaveDialog("/data/export.csv");
    WebforjTestbed.getCurrent().answerFileUploadDialog("photo.png");

    String chosen = OptionDialog.showFileChooserDialog("Open");
    String saved = OptionDialog.showFileSaveDialog("Save", "export.csv");
    UploadedFile uploaded = OptionDialog.showFileUploadDialog("Upload");

    assertEquals("/data/report.csv", chosen);
    assertEquals("/data/export.csv", saved);
    assertEquals("photo.png", uploaded.getClientName());
    assertEquals(
        List.of(ShownOptionDialog.Kind.FILE_CHOOSER, ShownOptionDialog.Kind.FILE_SAVE,
            ShownOptionDialog.Kind.FILE_UPLOAD),
        WebforjTestbed.getCurrent().getShownOptionDialogs().stream().map(ShownOptionDialog::getKind)
            .toList());
  }

  @Test
  void shouldCancelFileDialogsWithNull() {
    WebforjTestbed.getCurrent().answerFileChooserDialog(null);
    WebforjTestbed.getCurrent().answerFileUploadDialog(null);

    assertNull(OptionDialog.showFileChooserDialog("Open"));
    assertNull(OptionDialog.showFileUploadDialog("Upload"));
  }
}
