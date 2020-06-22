CREATE TABLE IF NOT EXISTS `bing_pict` (
`date`  int(11) UNSIGNED NOT NULL COMMENT '时间，格式：20170901' ,
`origin_url`  varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci NOT NULL DEFAULT '' COMMENT '来源，bing的原始地址' ,
`saved_path`  varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci NOT NULL DEFAULT '' COMMENT '本地保存的路径' ,
`copyright`  varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci NOT NULL DEFAULT '' COMMENT '描述' ,
`copyrightlink`  varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci NOT NULL DEFAULT '' COMMENT '描述搜索' ,
PRIMARY KEY (`date`)
)
ENGINE=InnoDB
DEFAULT CHARACTER SET=utf8 COLLATE=utf8_general_ci
ROW_FORMAT=DYNAMIC
;
