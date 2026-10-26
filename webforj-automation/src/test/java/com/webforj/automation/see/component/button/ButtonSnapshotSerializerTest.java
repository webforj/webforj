package com.webforj.automation.see.component.button;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.automation.see.Snapshot;
import com.webforj.automation.see.SnapshotOptions;
import com.webforj.component.badge.Badge;
import com.webforj.component.button.Button;
import com.webforj.component.icons.Icon;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class ButtonSnapshotSerializerTest {

  @ParameterizedTest
  @ValueSource(booleans = {false, true})
  void shouldPrintTheTextAtBothLevels(boolean browserState) {
    Button save = new Button("Save");

    assertEquals("- Button \"Save\"\n",
        Snapshot.render(save, new SnapshotOptions().setBrowserState(browserState)));
  }

  @Test
  void shouldPrintTheBadgeInItsSlot() {
    Button inbox = new Button("Inbox");
    inbox.setBadge(new Badge("5"));

    assertEquals("""
        - Button "Inbox":
          - Badge "5" [slot=badge]
        """, Snapshot.render(inbox));
  }

  @Test
  void shouldPrintTheIconSetInPlaceOfTheText() {
    Button bell = new Button();
    bell.setIcon(new Icon("bell", "tabler"));

    assertEquals("""
        - Button:
          - Icon [name=bell]
        """, Snapshot.render(bell));
  }
}
