package com.webforj.automation.see.component.icon;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.automation.see.Snapshot;
import com.webforj.component.icons.Icon;
import com.webforj.component.icons.IconButton;
import com.webforj.component.icons.IconExpanse;
import java.util.Locale;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

class IconSnapshotSerializerTest {

  @Test
  void shouldRestEveryIconAtExtraSmall() {
    assertEquals(IconExpanse.XSMALL, new Icon("bell", "tabler").getExpanse());
    assertEquals(IconExpanse.XSMALL, new IconButton("bell", "tabler").getExpanse());
  }

  @Test
  void shouldSkipTheExtraSmallExpanseAnIconRestsAt() {
    Icon bell = new Icon("bell", "tabler");

    assertEquals("- Icon [name=bell]\n", Snapshot.render(bell));
  }

  @ParameterizedTest
  @EnumSource(value = IconExpanse.class, names = {"MEDIUM", "XLARGE"})
  void shouldPrintAnyOtherExpanseIncludingMedium(IconExpanse expanse) {
    Icon bell = new Icon("bell", "tabler");
    bell.setExpanse(expanse);

    assertEquals("- Icon [name=bell] [expanse=" + expanse.name().toLowerCase(Locale.ROOT) + "]\n",
        Snapshot.render(bell));
  }
}
