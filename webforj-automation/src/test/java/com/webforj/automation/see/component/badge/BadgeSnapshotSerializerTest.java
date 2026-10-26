package com.webforj.automation.see.component.badge;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.automation.see.Snapshot;
import com.webforj.automation.see.SnapshotOptions;
import com.webforj.component.badge.Badge;
import com.webforj.component.badge.BadgeExpanse;
import java.util.Locale;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.ValueSource;

class BadgeSnapshotSerializerTest {

  @ParameterizedTest
  @ValueSource(booleans = {false, true})
  void shouldPrintTheTextAtBothLevels(boolean browserState) {
    Badge badge = new Badge("New");

    assertEquals("- Badge \"New\"\n",
        Snapshot.render(badge, new SnapshotOptions().setBrowserState(browserState)));
  }

  @Test
  void shouldSkipTheSmallExpanseTheBadgeRestsAt() {
    Badge badge = new Badge("New");
    badge.setExpanse(BadgeExpanse.SMALL);

    assertEquals("- Badge \"New\"\n", Snapshot.render(badge));
  }

  @ParameterizedTest
  @EnumSource(value = BadgeExpanse.class, names = {"MEDIUM", "XXXLARGE"})
  void shouldPrintAnyOtherExpanseIncludingMedium(BadgeExpanse expanse) {
    Badge badge = new Badge("New");
    badge.setExpanse(expanse);

    assertEquals("- Badge \"New\" [expanse=" + expanse.name().toLowerCase(Locale.ROOT) + "]\n",
        Snapshot.render(badge));
  }
}
