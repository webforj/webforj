package com.webforj.automation.see.component.infinitescroll;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.automation.see.Snapshot;
import com.webforj.automation.see.SnapshotOptions;
import com.webforj.component.infinitescroll.InfiniteScroll;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class InfiniteScrollSnapshotSerializerTest {

  @ParameterizedTest
  @ValueSource(booleans = {false, true})
  void shouldPrintTheTextAtBothLevels(boolean browserState) {
    InfiniteScroll feed = new InfiniteScroll("Loading more");

    assertEquals("- InfiniteScroll \"Loading more\"\n",
        Snapshot.render(feed, new SnapshotOptions().setBrowserState(browserState)));
  }
}
