SELECT
    item.*
  , GROUP_CONCAT(tag.name)
FROM
  item
INNER JOIN
  item_tag ON item_tag.item_id = item.id
INNER JOIN
  tag ON tag.id = item_tag.tag_id
GROUP BY
  item.id
;

-- Iteration with authors
-- There are some duplicates in authors
-- and potentially in tag names as well

SELECT DISTINCT
    item.*
  , GROUP_CONCAT(tag.name)
  , GROUP_CONCAT(author.first_name || ' ' || author.last_name)
FROM
  item
INNER JOIN
  item_tag ON item_tag.item_id = item.id
INNER JOIN
  tag ON tag.id = item_tag.tag_id
INNER JOIN
  item_author ON item_author.item_id = item.id
INNER JOIN
  author ON author.id = item_author.author_id
GROUP BY
    item.id
;
