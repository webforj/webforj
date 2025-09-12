package com.webforj.router.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.router.security.RouteAccessDecision.AccessDenialType;
import org.junit.jupiter.api.Test;

class RouteAccessDecisionTest {

  @Test
  void shouldGrantAccess() {
    RouteAccessDecision decision = RouteAccessDecision.grant();

    assertTrue(decision.isGranted());
    assertFalse(decision.isDenied());
    assertNull(decision.getReason());
    assertNull(decision.getDenialType());
    assertFalse(decision.isAuthenticationRequired());
    assertFalse(decision.isAccessDenied());
  }

  @Test
  void shouldDenyAuthenticationWithDefaultMessage() {
    RouteAccessDecision decision = RouteAccessDecision.denyAuthentication();

    assertFalse(decision.isGranted());
    assertTrue(decision.isDenied());
    assertEquals("Authentication required", decision.getReason());
    assertEquals(AccessDenialType.AUTHENTICATION_REQUIRED, decision.getDenialType());
    assertTrue(decision.isAuthenticationRequired());
    assertFalse(decision.isAccessDenied());
  }

  @Test
  void shouldDenyAuthenticationWithCustomReason() {
    String customReason = "Session expired";
    RouteAccessDecision decision = RouteAccessDecision.denyAuthentication(customReason);

    assertFalse(decision.isGranted());
    assertTrue(decision.isDenied());
    assertEquals(customReason, decision.getReason());
    assertEquals(AccessDenialType.AUTHENTICATION_REQUIRED, decision.getDenialType());
    assertTrue(decision.isAuthenticationRequired());
    assertFalse(decision.isAccessDenied());
  }

  @Test
  void shouldDenyWithInsufficientPermissions() {
    String reason = "Requires ADMIN role";
    RouteAccessDecision decision = RouteAccessDecision.deny(reason);

    assertFalse(decision.isGranted());
    assertTrue(decision.isDenied());
    assertEquals(reason, decision.getReason());
    assertEquals(AccessDenialType.ACCESS_DENIED, decision.getDenialType());
    assertFalse(decision.isAuthenticationRequired());
    assertTrue(decision.isAccessDenied());
  }

  @Test
  void shouldDenyWithCustomReason() {
    String reason = "Access denied from your location";
    RouteAccessDecision decision = RouteAccessDecision.deny(reason);

    assertFalse(decision.isGranted());
    assertTrue(decision.isDenied());
    assertEquals(reason, decision.getReason());
    assertEquals(AccessDenialType.ACCESS_DENIED, decision.getDenialType());
    assertFalse(decision.isAuthenticationRequired());
    assertTrue(decision.isAccessDenied());
  }

  @Test
  void shouldThrowWhenDenyAuthenticationWithNullReason() {
    assertThrows(IllegalArgumentException.class, () -> {
      RouteAccessDecision.denyAuthentication(null);
    });
  }

  @Test
  void shouldThrowWhenDenyWithNullReason() {
    assertThrows(IllegalArgumentException.class, () -> {
      RouteAccessDecision.deny(null);
    });
  }
}
