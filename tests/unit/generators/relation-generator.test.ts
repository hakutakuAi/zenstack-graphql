import { describe, test, expect, beforeEach } from 'bun:test'
import { UnifiedRelationGenerator } from '@generators/unified/unified-relation-generator'
import { TestFixtures, TestMockFactory, SpyOutputStrategy } from '../../helpers'

describe('UnifiedRelationGenerator', () => {
	let generator: UnifiedRelationGenerator
	let context: any
	let spyStrategy: SpyOutputStrategy

	beforeEach(() => {
		const baseContext = TestFixtures.createContext({
			includeRelations: true,
			models: [
				TestFixtures.createDataModel('User', [
					TestFixtures.createField('id', 'String'),
					TestFixtures.createField('name', 'String'),
					TestFixtures.createField('email', 'String'),
					TestFixtures.createRelationField('posts', 'Post', false, true),
					TestFixtures.createRelationField('profile', 'Profile'),
				]),
				TestFixtures.createDataModel('Post', [
					TestFixtures.createField('id', 'String'),
					TestFixtures.createField('title', 'String'),
					TestFixtures.createField('content', 'String'),
					TestFixtures.createRelationField('author', 'User'),
					TestFixtures.createRelationField('tags', 'Tag', false, true),
				]),
				TestFixtures.createDataModel('Profile', [
					TestFixtures.createField('id', 'String'),
					TestFixtures.createField('bio', 'String'),
					TestFixtures.createRelationField('user', 'User'),
				]),
				TestFixtures.createDataModel('Tag', [
					TestFixtures.createField('id', 'String'),
					TestFixtures.createField('name', 'String'),
					TestFixtures.createRelationField('posts', 'Post', false, true),
				]),
			],
		})

		const spyContext = TestMockFactory.createSpyUnifiedContext(baseContext)
		context = spyContext
		spyStrategy = spyContext.spy
		generator = new UnifiedRelationGenerator(context)
	})

	describe('Initialization', () => {
		test('should initialize successfully', () => {
			expect(generator).toBeDefined()
		})
	})

	describe('Relation Processing', () => {
		test('should return empty array as expected', () => {
			const result = generator.generate()

			expect(result).toBeDefined()
			expect(Array.isArray(result)).toBe(true)
			expect(result.length).toBe(0)
		})

		test('should process relations between models', () => {
			generator.generate()

			const relationCalls = spyStrategy.getCallsForMethod('processRelation')
			expect(relationCalls.length).toBeGreaterThan(0)

			const processedRelations = spyStrategy.getProcessedRelations()
			expect(processedRelations.length).toBeGreaterThan(0)
		})

		test('should extract relations from models correctly', () => {
			generator.generate()

			const relationCalls = spyStrategy.getCallsForMethod('processRelation')
			expect(relationCalls.length).toBe(6)

			const relationNames = relationCalls.map((call) => `${call.args[0].modelName}.${call.args[0].fieldName}`)
			expect(relationNames).toContain('User.posts')
			expect(relationNames).toContain('User.profile')
			expect(relationNames).toContain('Post.author')
			expect(relationNames).toContain('Post.tags')
			expect(relationNames).toContain('Profile.user')
			expect(relationNames).toContain('Tag.posts')
		})

		test('should populate relation field properties correctly', () => {
			generator.generate()

			const relationCalls = spyStrategy.getCallsForMethod('processRelation')
			const userPostsRelation = relationCalls.find((call) => call.args[0].modelName === 'User' && call.args[0].fieldName === 'posts')

			expect(userPostsRelation).toBeDefined()
			const relation = userPostsRelation!.args[0]
			expect(relation.modelName).toBe('User')
			expect(relation.fieldName).toBe('posts')
			expect(relation.targetModelName).toBe('Post')
			expect(relation.isList).toBe(true)
			expect(relation.isRequired).toBe(true)
		})

		test('should handle one-to-one relations correctly', () => {
			generator.generate()

			const relationCalls = spyStrategy.getCallsForMethod('processRelation')
			const userProfileRelation = relationCalls.find((call) => call.args[0].modelName === 'User' && call.args[0].fieldName === 'profile')

			expect(userProfileRelation).toBeDefined()
			const relation = userProfileRelation!.args[0]
			expect(relation.modelName).toBe('User')
			expect(relation.fieldName).toBe('profile')
			expect(relation.targetModelName).toBe('Profile')
			expect(relation.isList).toBe(false)
			expect(relation.isRequired).toBe(true)
		})

		test('should handle many-to-many relations correctly', () => {
			generator.generate()

			const relationCalls = spyStrategy.getCallsForMethod('processRelation')
			const postTagsRelation = relationCalls.find((call) => call.args[0].modelName === 'Post' && call.args[0].fieldName === 'tags')
			const tagPostsRelation = relationCalls.find((call) => call.args[0].modelName === 'Tag' && call.args[0].fieldName === 'posts')

			expect(postTagsRelation).toBeDefined()
			expect(tagPostsRelation).toBeDefined()

			const postTagsRel = postTagsRelation!.args[0]
			expect(postTagsRel.isList).toBe(true)
			expect(postTagsRel.targetModelName).toBe('Tag')

			const tagPostsRel = tagPostsRelation!.args[0]
			expect(tagPostsRel.isList).toBe(true)
			expect(tagPostsRel.targetModelName).toBe('Post')
		})
	})

	describe('Error Handling', () => {
		test('should handle models with no relations gracefully', () => {
			const noRelationContext = TestFixtures.createContext({
				models: [TestFixtures.createDataModel('Standalone', [TestFixtures.createField('id', 'String'), TestFixtures.createField('name', 'String')])],
			})

			const noRelationUnifiedContext = TestMockFactory.createSpyUnifiedContext(noRelationContext)
			const noRelationGenerator = new UnifiedRelationGenerator(noRelationUnifiedContext)

			const result = noRelationGenerator.generate()

			expect(result).toBeDefined()
			expect(result.length).toBe(0)

			const relationCalls = noRelationUnifiedContext.spy.getCallsForMethod('processRelation')
			expect(relationCalls.length).toBe(0)
		})

		test('should handle empty models gracefully', () => {
			const emptyContext = TestMockFactory.createUnifiedContext(
				TestFixtures.createContext({
					models: [],
				}),
			)
			const emptyGenerator = new UnifiedRelationGenerator(emptyContext)

			const result = emptyGenerator.generate()
			expect(result).toBeDefined()
			expect(result.length).toBe(0)
		})

		test('should skip abstract models', () => {
			const abstractContext = TestFixtures.createContext({
				models: [
					{
						...TestFixtures.createDataModel('AbstractModel', [TestFixtures.createRelationField('relation', 'OtherModel')]),
						isAbstract: true,
					},
					TestFixtures.createDataModel('ConcreteModel', [TestFixtures.createField('id', 'String')]),
				],
			})

			const abstractUnifiedContext = TestMockFactory.createSpyUnifiedContext(abstractContext)
			const abstractGenerator = new UnifiedRelationGenerator(abstractUnifiedContext)

			abstractGenerator.generate()

			const relationCalls = abstractUnifiedContext.spy.getCallsForMethod('processRelation')
			expect(relationCalls.length).toBe(0)
		})

		test('should handle missing target models gracefully', () => {
			const missingTargetContext = TestFixtures.createContext({
				models: [
					TestFixtures.createDataModel('User', [
						TestFixtures.createField('id', 'String'),
						TestFixtures.createRelationField('nonexistent', 'NonexistentModel'),
					]),
				],
			})

			const missingTargetUnifiedContext = TestMockFactory.createSpyUnifiedContext(missingTargetContext)
			const missingTargetGenerator = new UnifiedRelationGenerator(missingTargetUnifiedContext)

			expect(() => {
				const result = missingTargetGenerator.generate()
				expect(result).toBeDefined()
				expect(result.length).toBe(0)
			}).not.toThrow()

			const relationCalls = missingTargetUnifiedContext.spy.getCallsForMethod('processRelation')
			expect(relationCalls.length).toBe(0)
		})
	})

	describe('Field Filtering', () => {
		test('should process relations regardless of global includeRelations setting', () => {
			const noRelationsContext = TestFixtures.createContext({
				includeRelations: false,
				models: [
					TestFixtures.createDataModel('User', [
						TestFixtures.createField('id', 'String'),
						TestFixtures.createRelationField('posts', 'Post', false, true),
					]),
					TestFixtures.createDataModel('Post', [TestFixtures.createField('id', 'String'), TestFixtures.createRelationField('author', 'User')]),
				],
			})

			const noRelationsUnifiedContext = TestMockFactory.createSpyUnifiedContext(noRelationsContext)
			const noRelationsGenerator = new UnifiedRelationGenerator(noRelationsUnifiedContext)

			noRelationsGenerator.generate()

			const relationCalls = noRelationsUnifiedContext.spy.getCallsForMethod('processRelation')
			expect(relationCalls.length).toBe(2)
		})

		test('should only process valid relation fields', () => {
			const mixedContext = TestFixtures.createContext({
				includeRelations: true,
				models: [
					TestFixtures.createDataModel('Mixed', [
						TestFixtures.createField('id', 'String'),
						TestFixtures.createField('name', 'String'),
						TestFixtures.createField('age', 'Int'),
						TestFixtures.createRelationField('validRelation', 'User'),
					]),
					TestFixtures.createDataModel('User', [TestFixtures.createField('id', 'String')]),
				],
			})

			const mixedUnifiedContext = TestMockFactory.createSpyUnifiedContext(mixedContext)
			const mixedGenerator = new UnifiedRelationGenerator(mixedUnifiedContext)

			mixedGenerator.generate()

			const relationCalls = mixedUnifiedContext.spy.getCallsForMethod('processRelation')
			expect(relationCalls.length).toBe(1)

			const relation = relationCalls[0]!.args[0]
			expect(relation.fieldName).toBe('validRelation')
			expect(relation.targetModelName).toBe('User')
		})
	})

	describe('Relation Discovery', () => {
		test('should find related fields correctly', () => {
			generator.generate()

			const relationCalls = spyStrategy.getCallsForMethod('processRelation')
			const postAuthorRelation = relationCalls.find((call) => call.args[0].modelName === 'Post' && call.args[0].fieldName === 'author')
			const userPostsRelation = relationCalls.find((call) => call.args[0].modelName === 'User' && call.args[0].fieldName === 'posts')

			expect(postAuthorRelation).toBeDefined()
			expect(userPostsRelation).toBeDefined()

			const postAuthor = postAuthorRelation!.args[0]
			const userPosts = userPostsRelation!.args[0]

			expect(postAuthor.targetModelName).toBe('User')
			expect(userPosts.targetModelName).toBe('Post')
		})

		test('should handle bidirectional relations', () => {
			generator.generate()

			const relationCalls = spyStrategy.getCallsForMethod('processRelation')
			const userProfileRelation = relationCalls.find((call) => call.args[0].modelName === 'User' && call.args[0].fieldName === 'profile')
			const profileUserRelation = relationCalls.find((call) => call.args[0].modelName === 'Profile' && call.args[0].fieldName === 'user')

			expect(userProfileRelation).toBeDefined()
			expect(profileUserRelation).toBeDefined()

			const userProfile = userProfileRelation!.args[0]
			const profileUser = profileUserRelation!.args[0]

			expect(userProfile.targetModelName).toBe('Profile')
			expect(profileUser.targetModelName).toBe('User')
		})
	})

	describe('Complex Scenarios', () => {
		test('should handle self-referencing relations', () => {
			const selfRefContext = TestFixtures.createContext({
				includeRelations: true,
				models: [
					TestFixtures.createDataModel('Category', [
						TestFixtures.createField('id', 'String'),
						TestFixtures.createField('name', 'String'),
						TestFixtures.createRelationField('parent', 'Category', true),
						TestFixtures.createRelationField('children', 'Category', false, true),
					]),
				],
			})

			const selfRefUnifiedContext = TestMockFactory.createSpyUnifiedContext(selfRefContext)
			const selfRefGenerator = new UnifiedRelationGenerator(selfRefUnifiedContext)

			selfRefGenerator.generate()

			const relationCalls = selfRefUnifiedContext.spy.getCallsForMethod('processRelation')
			expect(relationCalls.length).toBe(2)

			const parentRelation = relationCalls.find((call) => call.args[0].fieldName === 'parent')
			const childrenRelation = relationCalls.find((call) => call.args[0].fieldName === 'children')

			expect(parentRelation).toBeDefined()
			expect(childrenRelation).toBeDefined()

			const parent = parentRelation!.args[0]
			const children = childrenRelation!.args[0]

			expect(parent.targetModelName).toBe('Category')
			expect(parent.isRequired).toBe(false)
			expect(children.targetModelName).toBe('Category')
			expect(children.isList).toBe(true)
		})

		test('should handle multiple models with complex relations', () => {
			const complexContext = TestFixtures.createContext({
				includeRelations: true,
				models: [
					TestFixtures.createDataModel('Organization', [
						TestFixtures.createField('id', 'String'),
						TestFixtures.createRelationField('users', 'User', false, true),
						TestFixtures.createRelationField('projects', 'Project', false, true),
					]),
					TestFixtures.createDataModel('User', [
						TestFixtures.createField('id', 'String'),
						TestFixtures.createRelationField('organization', 'Organization'),
						TestFixtures.createRelationField('projects', 'Project', false, true),
					]),
					TestFixtures.createDataModel('Project', [
						TestFixtures.createField('id', 'String'),
						TestFixtures.createRelationField('organization', 'Organization'),
						TestFixtures.createRelationField('users', 'User', false, true),
					]),
				],
			})

			const complexUnifiedContext = TestMockFactory.createSpyUnifiedContext(complexContext)
			const complexGenerator = new UnifiedRelationGenerator(complexUnifiedContext)

			complexGenerator.generate()

			const relationCalls = complexUnifiedContext.spy.getCallsForMethod('processRelation')
			expect(relationCalls.length).toBe(6)

			const relationFieldNames = relationCalls.map((call) => `${call.args[0].modelName}.${call.args[0].fieldName}`)
			expect(relationFieldNames).toContain('Organization.users')
			expect(relationFieldNames).toContain('Organization.projects')
			expect(relationFieldNames).toContain('User.organization')
			expect(relationFieldNames).toContain('User.projects')
			expect(relationFieldNames).toContain('Project.organization')
			expect(relationFieldNames).toContain('Project.users')
		})
	})

	describe('Relation Processing State', () => {
		test('should track processed relations', () => {
			generator.generate()

			const processedRelations = spyStrategy.getProcessedRelations()
			expect(processedRelations.length).toBeGreaterThan(0)

			expect(processedRelations).toContain('User-Post-posts')
			expect(processedRelations).toContain('Post-User-author')
			expect(processedRelations).toContain('User-Profile-profile')
			expect(processedRelations).toContain('Profile-User-user')
		})

		test('should be able to check if relations were processed', () => {
			generator.generate()

			expect(spyStrategy.hasProcessedRelation('User-Post-posts')).toBe(true)
			expect(spyStrategy.hasProcessedRelation('Post-User-author')).toBe(true)
			expect(spyStrategy.hasProcessedRelation('nonexistent-relation-key')).toBe(false)
		})
	})
})
