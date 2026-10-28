package com.webforj.automation.see.component.progressbar;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.automation.see.Snapshot;
import com.webforj.automation.see.SnapshotOptions;
import com.webforj.component.progressbar.ProgressBar;
import org.junit.jupiter.api.Test;

class ProgressBarSnapshotSerializerTest {

  @Test
  void shouldFlagTheIndeterminateUprightBarByDefault() {
    ProgressBar bar = createIndeterminateUprightBar();

    assertEquals("- ProgressBar [min=0] [max=100] [indeterminate] [vertical]\n",
        Snapshot.render(bar));
  }

  @Test
  void shouldFlagTheIndeterminateUprightBarWithBrowserState() {
    ProgressBar bar = createIndeterminateUprightBar();

    assertEquals("- ProgressBar [value=40] [min=0] [max=100] [indeterminate] [vertical]\n",
        Snapshot.render(bar, new SnapshotOptions().setBrowserState(true)));
  }

  @Test
  void shouldLeaveTheDeterminateLevelBarUnflagged() {
    ProgressBar bar = new ProgressBar();

    assertEquals("- ProgressBar [min=0] [max=100]\n", Snapshot.render(bar));
  }

  private static ProgressBar createIndeterminateUprightBar() {
    ProgressBar bar = new ProgressBar(40);
    bar.setIndeterminate(true);
    bar.setOrientation(ProgressBar.Orientation.VERTICAL);

    return bar;
  }
}
