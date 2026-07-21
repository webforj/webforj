package com.webforj.spring.routescan;

import com.webforj.router.annotation.Route;

/**
 * Sits alone in this package so a test can scan the package and get exactly one candidate.
 */
@Route("/scanned")
public class ScannedRouteView {
}
