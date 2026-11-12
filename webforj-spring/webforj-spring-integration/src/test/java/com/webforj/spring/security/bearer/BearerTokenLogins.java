package com.webforj.spring.security.bearer;

import java.time.Instant;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.oauth2.core.OAuth2AccessToken;
import org.springframework.security.oauth2.core.OAuth2AuthenticatedPrincipal;
import org.springframework.security.oauth2.jwt.Jwt;
import org.springframework.security.oauth2.server.resource.authentication.BearerTokenAuthentication;
import org.springframework.security.oauth2.server.resource.authentication.JwtAuthenticationToken;

final class BearerTokenLogins {

  private BearerTokenLogins() {}

  static JwtAuthenticationToken create(String name, String role) {
    List<GrantedAuthority> authorities = List.of(new SimpleGrantedAuthority(role));
    Jwt jwt = Jwt.withTokenValue(name + "-token").header("alg", "RS256").subject(name)
        .issuedAt(Instant.now()).expiresAt(Instant.now().plusSeconds(3600)).build();
    return new JwtAuthenticationToken(jwt, authorities);
  }

  static JwtAuthenticationToken create(String name, String role, String issuer) {
    List<GrantedAuthority> authorities = List.of(new SimpleGrantedAuthority(role));
    Jwt jwt = Jwt.withTokenValue(name + "-token").header("alg", "RS256").subject(name)
        .issuer(issuer).issuedAt(Instant.now()).expiresAt(Instant.now().plusSeconds(3600)).build();
    return new JwtAuthenticationToken(jwt, authorities);
  }

  static JwtAuthenticationToken createWithoutName(String role) {
    return createWithoutName(role, role + "-token");
  }

  static JwtAuthenticationToken createWithoutName(String role, String tokenValue) {
    List<GrantedAuthority> authorities = List.of(new SimpleGrantedAuthority(role));
    Jwt jwt = Jwt.withTokenValue(tokenValue).header("alg", "RS256").issuedAt(Instant.now())
        .expiresAt(Instant.now().plusSeconds(3600)).build();
    return new JwtAuthenticationToken(jwt, authorities);
  }

  static BearerTokenAuthentication createWithNullName(String tokenValue) {
    OAuth2AccessToken token = new OAuth2AccessToken(OAuth2AccessToken.TokenType.BEARER, tokenValue,
        Instant.now(), Instant.now().plusSeconds(3600));
    return new BearerTokenAuthentication(new NullNamePrincipal(), token, List.of());
  }

  static JwtAuthenticationToken createExpired(String name) {
    Jwt jwt = Jwt.withTokenValue(name + "-token").header("alg", "RS256").subject(name)
        .issuedAt(Instant.now().minusSeconds(7200)).expiresAt(Instant.now().minusSeconds(3600))
        .build();
    return new JwtAuthenticationToken(jwt, List.of(new SimpleGrantedAuthority("ROLE_USER")));
  }

  private static final class NullNamePrincipal implements OAuth2AuthenticatedPrincipal {

    @Override
    public Map<String, Object> getAttributes() {
      return Map.of();
    }

    @Override
    public Collection<? extends GrantedAuthority> getAuthorities() {
      return List.of();
    }

    @Override
    public String getName() {
      return null;
    }
  }
}
