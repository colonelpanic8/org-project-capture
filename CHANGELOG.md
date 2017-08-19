# Change Log

## [v1.1.0](https://github.com/IvanMalison/org-projectile/tree/v1.1.0)

[Full Changelog](https://github.com/IvanMalison/org-projectile/compare/v1.0.0...HEAD)

**Closed issues:**

- Creating a TODO fails with error 'Wrong type argument: number-or-marker-p, nil' [\#33](https://github.com/IvanMalison/org-projectile/issues/33)
- org-projectile-location-for-project fails with wrong-number-of-arguments [\#32](https://github.com/IvanMalison/org-projectile/issues/32)

## [v1.0.0](https://github.com/IvanMalison/org-projectile/tree/v1.0.0) (2017-08-01)
[Full Changelog](https://github.com/IvanMalison/org-projectile/compare/v0.2.6...v1.0.0)

**Closed issues:**

- `\(org-projectile-per-repo\)` produces error [\#31](https://github.com/IvanMalison/org-projectile/issues/31)
- Getting error "org-projectile:insert-or-goto-heading: Wrong number of arguments: \#\[\(&optional arg invisible-ok\)" [\#29](https://github.com/IvanMalison/org-projectile/issues/29)
- \(invalid-function helm-build-sync-source\) [\#14](https://github.com/IvanMalison/org-projectile/issues/14)

**Merged pull requests:**

- org-projectile:project-name-to-location-alist: use true names [\#30](https://github.com/IvanMalison/org-projectile/pull/30) ([braham-snyder](https://github.com/braham-snyder))

## [v0.2.6](https://github.com/IvanMalison/org-projectile/tree/v0.2.6) (2016-12-05)
[Full Changelog](https://github.com/IvanMalison/org-projectile/compare/v0.2.5...v0.2.6)

**Closed issues:**

- Failure in org-projectile:project-todo-completing-read [\#27](https://github.com/IvanMalison/org-projectile/issues/27)
- Obsolete usage of pcache-repository [\#24](https://github.com/IvanMalison/org-projectile/issues/24)
- Byte compiling error: void variable: occ-build-capture-template \(broken in emacs 24\) [\#22](https://github.com/IvanMalison/org-projectile/issues/22)
- user-error: Not on a heading [\#20](https://github.com/IvanMalison/org-projectile/issues/20)

**Merged pull requests:**

- Remove pcache as a package dependency [\#26](https://github.com/IvanMalison/org-projectile/pull/26) ([yyadavalli](https://github.com/yyadavalli))
- Remove org-projectile:path-to-category [\#25](https://github.com/IvanMalison/org-projectile/pull/25) ([mdorman](https://github.com/mdorman))
- Load dash explicitly for using '--some' [\#23](https://github.com/IvanMalison/org-projectile/pull/23) ([syohex](https://github.com/syohex))

## [v0.2.5](https://github.com/IvanMalison/org-projectile/tree/v0.2.5) (2016-11-04)
[Full Changelog](https://github.com/IvanMalison/org-projectile/compare/v0.2.4...v0.2.5)

## [v0.2.4](https://github.com/IvanMalison/org-projectile/tree/v0.2.4) (2016-11-03)
[Full Changelog](https://github.com/IvanMalison/org-projectile/compare/v0.2.3...v0.2.4)

**Closed issues:**

- org-insert-subheading: Buffer is read-only: [\#21](https://github.com/IvanMalison/org-projectile/issues/21)
- org-projectile:known-projects shouldn't load remote projects [\#10](https://github.com/IvanMalison/org-projectile/issues/10)

## [v0.2.3](https://github.com/IvanMalison/org-projectile/tree/v0.2.3) (2016-06-17)
[Full Changelog](https://github.com/IvanMalison/org-projectile/compare/v0.2.2...v0.2.3)

## [v0.2.2](https://github.com/IvanMalison/org-projectile/tree/v0.2.2) (2016-06-17)
[Full Changelog](https://github.com/IvanMalison/org-projectile/compare/v0.2.1...v0.2.2)

**Merged pull requests:**

- Miscellaneous [\#19](https://github.com/IvanMalison/org-projectile/pull/19) ([TheBB](https://github.com/TheBB))

## [v0.2.1](https://github.com/IvanMalison/org-projectile/tree/v0.2.1) (2016-06-15)
[Full Changelog](https://github.com/IvanMalison/org-projectile/compare/v0.2.0...v0.2.1)

**Closed issues:**

- Customization of todo entry properties [\#18](https://github.com/IvanMalison/org-projectile/issues/18)
- Per-repo mode gives nested headings [\#17](https://github.com/IvanMalison/org-projectile/issues/17)
- Helm requirement [\#15](https://github.com/IvanMalison/org-projectile/issues/15)
- file-truename\(nil\) [\#13](https://github.com/IvanMalison/org-projectile/issues/13)
- Perhaps tag a release? [\#8](https://github.com/IvanMalison/org-projectile/issues/8)
- Don't run file-truename on nil \(handle deleted projects gracefully\) [\#7](https://github.com/IvanMalison/org-projectile/issues/7)

**Merged pull requests:**

- Remove helm from requirments. Fixes issue \#15 [\#16](https://github.com/IvanMalison/org-projectile/pull/16) ([jtamagnan](https://github.com/jtamagnan))
- Fix org-projectile:capture-for-current-project to be a command [\#12](https://github.com/IvanMalison/org-projectile/pull/12) ([blallau](https://github.com/blallau))
- Cleanup compiler warnings [\#11](https://github.com/IvanMalison/org-projectile/pull/11) ([bookest](https://github.com/bookest))
- Add a Gitter chat badge to README.org [\#9](https://github.com/IvanMalison/org-projectile/pull/9) ([gitter-badger](https://github.com/gitter-badger))

## [v0.2.0](https://github.com/IvanMalison/org-projectile/tree/v0.2.0) (2015-09-03)
**Closed issues:**

- Insertion into subheading in single-file mode may be broken [\#6](https://github.com/IvanMalison/org-projectile/issues/6)
- Saving todos in project specific org-file [\#4](https://github.com/IvanMalison/org-projectile/issues/4)
- org-projectile:project-root-of-filepath: Wrong type argument: arrayp, nil [\#2](https://github.com/IvanMalison/org-projectile/issues/2)

**Merged pull requests:**

- Added a capture-for-current-project function [\#3](https://github.com/IvanMalison/org-projectile/pull/3) ([luxbock](https://github.com/luxbock))
- Remove org dependency [\#1](https://github.com/IvanMalison/org-projectile/pull/1) ([bbigras](https://github.com/bbigras))



\* *This Change Log was automatically generated by [github_changelog_generator](https://github.com/skywinder/Github-Changelog-Generator)*
