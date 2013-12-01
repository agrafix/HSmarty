{* test *}
<html>
<head>
  <title>{$title}</title>
</head>
<body>
{foreach $list as $item}
      <li>{$item}</li>
      <li>{$item@last}</li>
{/foreach}
</body>
</html>
