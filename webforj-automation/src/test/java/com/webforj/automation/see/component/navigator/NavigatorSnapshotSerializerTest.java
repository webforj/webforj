package com.webforj.automation.see.component.navigator;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.automation.see.Snapshot;
import com.webforj.automation.see.SnapshotOptions;
import com.webforj.component.navigator.Navigator;
import org.junit.jupiter.api.Test;

class NavigatorSnapshotSerializerTest {

  @Test
  void shouldDescribeThePageWithoutTheMovesByDefault() {
    Navigator pages = new Navigator(100, 10);

    assertEquals("- Navigator [page=1] [pages=10]\n", Snapshot.render(pages));
  }

  @Test
  void shouldAddTheMovesWithBrowserState() {
    Navigator pages = new Navigator(100, 10);

    assertEquals("""
        - Navigator [page=1] [pages=10]:
          - Move "first"
          - Move "previous"
          - Move "next"
          - Move "last"
        """, Snapshot.render(pages, new SnapshotOptions().setBrowserState(true)));
  }

  @Test
  void shouldDisableTheBackwardMovesOnTheFirstPageWithBrowserState() {
    Navigator pages = new Navigator(100, 10);
    pages.setAutoDisable(true);

    assertEquals("""
        - Navigator [page=1] [pages=10]:
          - Move "first" [disabled]
          - Move "previous" [disabled]
          - Move "next"
          - Move "last"
        """, Snapshot.render(pages, new SnapshotOptions().setBrowserState(true)));
  }

  @Test
  void shouldDisableTheForwardMovesOnTheLastPageWithBrowserState() {
    Navigator pages = new Navigator(100, 10);
    pages.setAutoDisable(true);
    pages.getPaginator().setCurrent(10);

    assertEquals("""
        - Navigator [page=10] [pages=10]:
          - Move "first"
          - Move "previous"
          - Move "next" [disabled]
          - Move "last" [disabled]
        """, Snapshot.render(pages, new SnapshotOptions().setBrowserState(true)));
  }

  @Test
  void shouldLeaveOutTheHiddenMoveWithBrowserState() {
    Navigator pages = new Navigator(100, 10);
    pages.setVisible(false, Navigator.Part.FIRST_BUTTON);

    assertEquals("""
        - Navigator [page=1] [pages=10]:
          - Move "previous"
          - Move "next"
          - Move "last"
        """, Snapshot.render(pages, new SnapshotOptions().setBrowserState(true)));
  }

  @Test
  void shouldLeaveOutAllMovesWhenTheMainButtonsAreHiddenWithBrowserState() {
    Navigator pages = new Navigator(100, 10);
    pages.setHideMainButtons(true);

    assertEquals("- Navigator [page=1] [pages=10]\n",
        Snapshot.render(pages, new SnapshotOptions().setBrowserState(true)));
  }

  @Test
  void shouldPrintTheTextByDefault() {
    Navigator pages = new Navigator(100, 10);
    pages.setText("Results");

    assertEquals("- Navigator \"Results\" [page=1] [pages=10]\n", Snapshot.render(pages));
  }

  @Test
  void shouldListThePageButtonsOfTheNumberedLayoutByDefault() {
    Navigator pages = new Navigator(30, 10, Navigator.Layout.PAGES);
    pages.getPaginator().setCurrent(2);

    assertEquals("""
        - Navigator [page=2] [pages=3]:
          - Page "1"
          - Page "2" [selected]
          - Page "3"
        """, Snapshot.render(pages));
  }

  @Test
  void shouldListThePageButtonsBetweenTheMovesWithBrowserState() {
    Navigator pages = new Navigator(20, 10, Navigator.Layout.PAGES);

    assertEquals("""
        - Navigator [page=1] [pages=2]:
          - Move "first"
          - Move "previous"
          - Page "1" [selected]
          - Page "2"
          - Move "next"
          - Move "last"
        """, Snapshot.render(pages, new SnapshotOptions().setBrowserState(true)));
  }
}
