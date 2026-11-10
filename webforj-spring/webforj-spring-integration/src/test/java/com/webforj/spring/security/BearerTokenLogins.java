package com.webforj.spring.security;

import java.time.Instant;
import java.util.List;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.oauth2.jwt.Jwt;
import org.springframework.security.oauth2.server.resource.authentication.JwtAuthenticationToken;

final class BearerTokenLogins {

  private BearerTokenLogins() {}

  static JwtAuthenticationToken create(String name, String role) {
    List<GrantedAuthority> authorities = List.of(new SimpleGrantedAuthority(role));
    Jwt jwt = Jwt.withTokenValue(name + "-token").header("alg", "RS256").subject(name)
        .issuedAt(Instant.now()).expiresAt(Instant.now().plusSeconds(3600)).build();
    return new JwtAuthenticationToken(jwt, authorities);
  }
}
