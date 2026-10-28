package com.webforj.testbed.fixture.security;

import com.webforj.component.Composite;
import com.webforj.component.html.elements.Div;
import com.webforj.router.annotation.Route;
import jakarta.annotation.security.DenyAll;

/** Provides a route that denies all access for security tests. */
@Route("vault")
@DenyAll
public class SecureVaultView extends Composite<Div> {
}
