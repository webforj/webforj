package com.webforj.testbed.fixture.security;

import com.webforj.component.Composite;
import com.webforj.component.html.elements.Div;
import com.webforj.router.annotation.Route;
import jakarta.annotation.security.RolesAllowed;

/** Provides an admin-only route for security tests. */
@Route("admin")
@RolesAllowed("ADMIN")
public class SecureAdminView extends Composite<Div> {
}
