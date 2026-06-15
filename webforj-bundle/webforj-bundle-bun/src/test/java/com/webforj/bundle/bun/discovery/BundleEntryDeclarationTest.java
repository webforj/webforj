package com.webforj.bundle.bun.discovery;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;
import org.junit.jupiter.api.Test;

class BundleEntryDeclarationTest {

  @Test
  void shouldStartWithNoOwners() {
    assertTrue(new BundleEntryDeclaration().getOwners().isEmpty());
  }

  @Test
  void shouldAddOwnersOneByOne() {
    BundleEntryDeclaration declaration =
        new BundleEntryDeclaration().addOwner("com.acme.CardView").addOwner("com.acme.PanelView");

    assertEquals(List.of("com.acme.CardView", "com.acme.PanelView"), declaration.getOwners());
  }

  @Test
  void shouldSetOwnersFromAList() {
    BundleEntryDeclaration declaration =
        new BundleEntryDeclaration().setOwners(List.of("com.acme.CardView"));

    assertEquals(List.of("com.acme.CardView"), declaration.getOwners());
  }

  @Test
  void shouldDefendOwnersAgainstNull() {
    assertTrue(new BundleEntryDeclaration().setOwners(null).getOwners().isEmpty());
  }
}
