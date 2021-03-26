---
layout: default
title: Blog
---

<ul>
  {% for post in site.posts %}
    <li style="margin-bottom:48px">
      <h2><a href="{{ post.url }}">{{ post.title }}</a></h2>
      <p>{{ post.date }} by {{ post.author }} </p>
      {{ post.excerpt }}
    </li>
  {% endfor %}
</ul>
