package com.webforj.automation.see.component.dialog;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.automation.see.Snapshot;
import com.webforj.automation.see.SnapshotOptions;
import com.webforj.component.dialog.Dialog;
import org.junit.jupiter.api.Test;

class DialogSnapshotSerializerTest {

  @Test
  void shouldDescribeTheOpenedDialogWithoutItsStateByDefault() {
    Dialog dialog = new Dialog();
    dialog.open();

    assertEquals("- Dialog\n", Snapshot.render(dialog));
  }

  @Test
  void shouldFlagTheOpenedDialogWithBrowserState() {
    Dialog dialog = new Dialog();
    dialog.open();

    assertEquals("- Dialog [opened]\n",
        Snapshot.render(dialog, new SnapshotOptions().setBrowserState(true)));
  }

  @Test
  void shouldLeaveTheClosedDialogUnflaggedWithBrowserState() {
    Dialog dialog = new Dialog();
    dialog.close();

    assertEquals("- Dialog\n",
        Snapshot.render(dialog, new SnapshotOptions().setBrowserState(true)));
  }
}
