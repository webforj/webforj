package com.webforj.spring.push;


import com.typesafe.config.ConfigFactory;
import com.webforj.push.PushConfiguration;
import com.webforj.push.PushSender;
import com.webforj.spring.SpringConfigurationProperties;
import java.util.HashMap;
import java.util.Map;
import org.springframework.boot.autoconfigure.AutoConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnExpression;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;

/**
 * Exposes the push sender of the deployment as a bean when the {@code webforj.push} properties are
 * set.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoConfiguration
@ConditionalOnClass(PushSender.class)
@ConditionalOnExpression("'${webforj.push.public-key:}${webforj.push.private-key:}"
    + "${webforj.push.subject:}' != ''")
@EnableConfigurationProperties(SpringConfigurationProperties.class)
public class PushAutoConfiguration {

  /**
   * Creates the sender from the push properties.
   *
   * @param properties the webforJ configuration of the application
   * @return the sender
   *
   * @throws WebforjPushException when the push properties are only partially set or not valid
   */
  @Bean
  @ConditionalOnMissingBean
  PushSender webforjPushSender(SpringConfigurationProperties properties) {
    SpringConfigurationProperties.Push push = properties.getPush();
    Map<String, String> values = new HashMap<>();
    putIfSet(values, PushConfiguration.PUBLIC_KEY, push.getPublicKey());
    putIfSet(values, PushConfiguration.PRIVATE_KEY, push.getPrivateKey());
    putIfSet(values, PushConfiguration.SUBJECT, push.getSubject());

    return new PushSender(PushConfiguration.require(ConfigFactory.parseMap(values)));
  }

  private static void putIfSet(Map<String, String> values, String key, String value) {
    if (value != null) {
      values.put(key, value);
    }
  }
}
