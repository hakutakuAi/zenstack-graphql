import { expect, test, describe, beforeAll } from 'bun:test'
import { existsSync, readFileSync } from 'fs'
import { join } from 'path'

describe('TypeGraphQL Test Example', () => {
	const schemaPath = join(__dirname, './schema.ts')
	let schemaContent: string

	beforeAll(() => {
		expect(existsSync(schemaPath)).toBe(true)
		schemaContent = readFileSync(schemaPath, 'utf8')
	})

	test('Generated TypeScript file contains imports', () => {
		expect(schemaContent).toContain('import { ObjectType, Field, ID, Int, Float, registerEnumType, InputType, ArgsType } from "type-graphql"')
		expect(schemaContent).toContain('import { GraphQLJSON } from "graphql-scalars"')
		expect(schemaContent).toContain('import "reflect-metadata"')
	})

	test('Generated file contains User class with ObjectType decorator', () => {
		expect(schemaContent).toContain('@ObjectType()')
		expect(schemaContent).toContain('export class User')
		expect(schemaContent).toContain('@Field(() => String)')
		expect(schemaContent).toContain('id!: string')
		expect(schemaContent).toContain('name!: string')
		expect(schemaContent).toContain('email!: string')
	})

	test('Generated file contains Post class with ObjectType decorator', () => {
		expect(schemaContent).toContain('export class Post')
		expect(schemaContent).toContain('title!: string')
		expect(schemaContent).toContain('content!: string')
		expect(schemaContent).toContain('published!: boolean')
	})

	test('Generated file contains Category and Comment classes', () => {
		expect(schemaContent).toContain('export class Category')
		expect(schemaContent).toContain('export class Comment')
		expect(schemaContent).toContain('export class PostCategory')
	})

	test('Generated file contains proper Field decorators', () => {
		expect(schemaContent).toContain('@Field(() => String)')
		expect(schemaContent).toContain('@Field(() => Boolean)')
		expect(schemaContent).toContain('@Field(() => Date)')
	})

	test('Generated file handles optional fields', () => {
		expect(schemaContent).toContain('nullable: true')
		expect(schemaContent).toContain('bio?: string')
	})

	test('Generated file has proper property assertions', () => {
		expect(schemaContent).toContain('id!: string')
		expect(schemaContent).toContain('name!: string')
		expect(schemaContent).toContain('email!: string')
	})

	test('Generated TypeScript code is syntactically valid', () => {
		const openBraces = (schemaContent.match(/\{/g) || []).length
		const closeBraces = (schemaContent.match(/\}/g) || []).length
		expect(openBraces).toBe(closeBraces)

		const openParens = (schemaContent.match(/\(/g) || []).length
		const closeParens = (schemaContent.match(/\)/g) || []).length
		expect(openParens).toBe(closeParens)
	})

	// Filter Input Types Tests
	describe('Filter Input Types', () => {
		test('Contains base filter input types', () => {
			expect(schemaContent).toContain('export class NumericFilterInput')
			expect(schemaContent).toContain('export class DateTimeFilterInput')
			expect(schemaContent).toContain('export class StringFilterInput')
			expect(schemaContent).toContain('export class BooleanFilterInput')
		})

		test('Contains model-specific filter input types', () => {
			expect(schemaContent).toContain('export class UserFilterInput')
			expect(schemaContent).toContain('export class PostFilterInput')
			expect(schemaContent).toContain('export class CategoryFilterInput')
			expect(schemaContent).toContain('export class PostCategoryFilterInput')
			expect(schemaContent).toContain('export class CommentFilterInput')
		})

		test('Filter inputs have proper InputType decorators', () => {
			const filterInputMatches = schemaContent.match(/@InputType\(\)\s*\n\s*export class \w+FilterInput/g)
			expect(filterInputMatches).not.toBeNull()
			expect(filterInputMatches!.length).toBeGreaterThan(4)
		})

		test('StringFilterInput has correct fields and types', () => {
			expect(schemaContent).toContain('equals?: string | undefined')
			expect(schemaContent).toContain('not?: string | undefined')
			expect(schemaContent).toContain('in?: string[] | undefined')
			expect(schemaContent).toContain('notIn?: string[] | undefined')
			expect(schemaContent).toContain('contains?: string | undefined')
			expect(schemaContent).toContain('startsWith?: string | undefined')
			expect(schemaContent).toContain('endsWith?: string | undefined')
		})

		test('NumericFilterInput has correct fields and types', () => {
			expect(schemaContent).toContain('@Field(() => Float, { nullable: true })')
			expect(schemaContent).toContain('equals?: number | undefined')
			expect(schemaContent).toContain('gt?: number | undefined')
			expect(schemaContent).toContain('lt?: number | undefined')
		})

		test('BooleanFilterInput has correct fields', () => {
			expect(schemaContent).toContain('export class BooleanFilterInput')
			expect(schemaContent).toContain('@Field(() => Boolean, { nullable: true })')
		})

		test('Model filter inputs have AND/OR operators', () => {
			expect(schemaContent).toContain('AND?: UserFilterInput[] | undefined')
			expect(schemaContent).toContain('OR?: UserFilterInput[] | undefined')
			expect(schemaContent).toContain('AND?: PostFilterInput[] | undefined')
			expect(schemaContent).toContain('OR?: PostFilterInput[] | undefined')
		})
	})

	// Sort Input Types Tests
	describe('Sort Input Types', () => {
		test('Contains SortDirection enum', () => {
			expect(schemaContent).toContain('export enum SortDirection')
			expect(schemaContent).toContain('ASC = "ASC"')
			expect(schemaContent).toContain('DESC = "DESC"')
			expect(schemaContent).toContain('registerEnumType(SortDirection')
		})

		test('Contains model sort input types', () => {
			expect(schemaContent).toContain('export class UserSortInput')
			expect(schemaContent).toContain('export class PostSortInput')
			expect(schemaContent).toContain('export class CategorySortInput')
			expect(schemaContent).toContain('export class CommentSortInput')
		})

		test('Sort inputs have proper field types', () => {
			expect(schemaContent).toContain('@Field(() => SortDirection, { nullable: true })')
			expect(schemaContent).toContain('createdAt?: SortDirection | undefined')
			expect(schemaContent).toContain('updatedAt?: SortDirection | undefined')
		})

		test('PostSortInput includes viewCount field', () => {
			expect(schemaContent).toContain('viewCount?: SortDirection | undefined')
		})
	})

	// Pagination Input Types Tests
	describe('Pagination Input Types', () => {
		test('Contains pagination input types', () => {
			expect(schemaContent).toContain('export class ForwardPaginationInput')
			expect(schemaContent).toContain('export class BackwardPaginationInput')
			expect(schemaContent).toContain('export class PaginationInput')
		})

		test('ForwardPaginationInput has correct fields', () => {
			expect(schemaContent).toContain('first?: number | undefined')
			expect(schemaContent).toContain('after?: string | undefined')
		})

		test('BackwardPaginationInput has correct fields', () => {
			expect(schemaContent).toContain('last?: number | undefined')
			expect(schemaContent).toContain('before?: string | undefined')
		})

		test('PaginationInput combines all pagination fields', () => {
			const paginationInputMatch = schemaContent.match(/export class PaginationInput[\s\S]*?(?=export class|\n\n@|$)/)
			expect(paginationInputMatch).not.toBeNull()
			const paginationInputSection = paginationInputMatch![0]
			expect(paginationInputSection).toContain('first?: number')
			expect(paginationInputSection).toContain('after?: string')
			expect(paginationInputSection).toContain('last?: number')
			expect(paginationInputSection).toContain('before?: string')
		})
	})

	// Connection Types Tests
	describe('Connection Types', () => {
		test('Contains PageInfo class', () => {
			expect(schemaContent).toContain('export class PageInfo')
			expect(schemaContent).toContain('hasNextPage!: boolean')
			expect(schemaContent).toContain('hasPreviousPage!: boolean')
			expect(schemaContent).toContain('startCursor?: string | undefined')
			expect(schemaContent).toContain('endCursor?: string | undefined')
		})

		test('Contains edge classes for all models', () => {
			expect(schemaContent).toContain('export class UserEdge')
			expect(schemaContent).toContain('export class PostEdge')
			expect(schemaContent).toContain('export class CategoryEdge')
			expect(schemaContent).toContain('export class PostCategoryEdge')
			expect(schemaContent).toContain('export class CommentEdge')
		})

		test('Contains connection classes for all models', () => {
			expect(schemaContent).toContain('export class UserConnection')
			expect(schemaContent).toContain('export class PostConnection')
			expect(schemaContent).toContain('export class CategoryConnection')
			expect(schemaContent).toContain('export class PostCategoryConnection')
			expect(schemaContent).toContain('export class CommentConnection')
		})

		test('Edge classes have correct structure', () => {
			const userEdgeMatch = schemaContent.match(/export class UserEdge[\s\S]*?(?=export class|\n\n@|$)/)
			expect(userEdgeMatch).not.toBeNull()
			const userEdgeSection = userEdgeMatch![0]
			expect(userEdgeSection).toContain('node!: User')
			expect(userEdgeSection).toContain('cursor!: string')
			expect(userEdgeSection).toContain('@Field(() => User)')
			expect(userEdgeSection).toContain('@Field(() => String)')
		})

		test('Connection classes have correct structure', () => {
			const userConnectionMatch = schemaContent.match(/export class UserConnection[\s\S]*?(?=export class|\n\n@|$)/)
			expect(userConnectionMatch).not.toBeNull()
			const userConnectionSection = userConnectionMatch![0]
			expect(userConnectionSection).toContain('pageInfo!: PageInfo')
			expect(userConnectionSection).toContain('edges!: UserEdge[]')
			expect(userConnectionSection).toContain('totalCount!: number')
			expect(userConnectionSection).toContain('@Field(() => PageInfo)')
			expect(userConnectionSection).toContain('@Field(() => [UserEdge])')
			expect(userConnectionSection).toContain('@Field(() => Int)')
		})
	})

	// TypeScript Types Validation
	describe('TypeScript Types Validation', () => {
		test('Uses proper TypeScript number types instead of GraphQL Float', () => {
			const numberFieldMatches = schemaContent.match(/: number/g)
			expect(numberFieldMatches).not.toBeNull()
			expect(numberFieldMatches!.length).toBeGreaterThan(5)
		})

		test('Uses proper TypeScript string array types', () => {
			expect(schemaContent).toContain('string[] | undefined')
			expect(schemaContent).not.toContain('String![] | undefined')
		})

		test('Optional fields have proper TypeScript union with undefined', () => {
			expect(schemaContent).toContain('| undefined')
		})

		test('Required fields have exclamation marks', () => {
			expect(schemaContent).toContain('!: string')
			expect(schemaContent).toContain('!: boolean')
			expect(schemaContent).toContain('!: number')
		})
	})

	// Relations and PostCategory Tests
	describe('Relations and Custom Names', () => {
		test('PostCategory uses correct name from @@graphql.name attribute', () => {
			expect(schemaContent).toContain('export class PostCategory')
			expect(schemaContent).not.toContain('export class CategoryOnPost')
		})

		test('Relations reference correct type names', () => {
			expect(schemaContent).toContain('@Field(() => [PostCategory])')
			expect(schemaContent).toContain('categories!: PostCategory[]')
		})

		test('All models have proper relation fields', () => {
			expect(schemaContent).toContain('@Field(() => [Post])')
			expect(schemaContent).toContain('posts!: Post[]')
			expect(schemaContent).toContain('@Field(() => [Comment])')
			expect(schemaContent).toContain('comments!: Comment[]')
			expect(schemaContent).toContain('@Field(() => User)')
			expect(schemaContent).toContain('author!: User')
		})

		test('PostCategory has correct foreign key fields', () => {
			const postCategoryMatch = schemaContent.match(/export class PostCategory[\s\S]*?(?=export class|$)/)
			expect(postCategoryMatch).not.toBeNull()
			const postCategorySection = postCategoryMatch![0]
			expect(postCategorySection).toContain('postId!: string')
			expect(postCategorySection).toContain('categoryId!: string')
			expect(postCategorySection).toContain('@Field(() => String)')
		})

		test('User has proper bi-directional relations', () => {
			const userMatch = schemaContent.match(/export class User[\s\S]*?(?=export class|$)/)
			expect(userMatch).not.toBeNull()
			const userSection = userMatch![0]
			expect(userSection).toContain('posts!: Post[]')
			expect(userSection).toContain('comments!: Comment[]')
			expect(userSection).toContain('@Field(() => [Post])')
			expect(userSection).toContain('@Field(() => [Comment])')
		})
	})

	// Comprehensive Validation Tests
	describe('Comprehensive Schema Validation', () => {
		test('All classes have proper ObjectType or InputType decorators', () => {
			const classMatches = schemaContent.match(/export class \w+/g)
			expect(classMatches).not.toBeNull()
			expect(classMatches!.length).toBeGreaterThan(20)

			const decoratorMatches = schemaContent.match(/@(ObjectType|InputType)\(\)/g)
			expect(decoratorMatches).not.toBeNull()
			expect(decoratorMatches!.length).toBeGreaterThan(20)
		})

		test('All non-nullable fields have exclamation marks', () => {
			const requiredFieldMatches = schemaContent.match(/!: (string|number|boolean|Date)/g)
			expect(requiredFieldMatches).not.toBeNull()
			expect(requiredFieldMatches!.length).toBeGreaterThan(10)
		})

		test('All nullable fields have proper undefined union types', () => {
			const nullableFieldMatches = schemaContent.match(/\?: \w+(\[\])? \| undefined/g)
			expect(nullableFieldMatches).not.toBeNull()
			expect(nullableFieldMatches!.length).toBeGreaterThan(5)
		})

		test('All array fields use proper TypeScript array syntax', () => {
			const arrayFieldMatches = schemaContent.match(/!: \w+\[\]/g)
			expect(arrayFieldMatches).not.toBeNull()
			expect(arrayFieldMatches!.length).toBeGreaterThan(5)
		})

		test('All Field decorators have proper GraphQL types', () => {
			expect(schemaContent).toContain('@Field(() => String)')
			expect(schemaContent).toContain('@Field(() => Boolean)')
			expect(schemaContent).toContain('@Field(() => Date)')
			expect(schemaContent).toContain('@Field(() => Int)')
			expect(schemaContent).toContain('@Field(() => Float)')
		})

		test('Connection types follow GraphQL Relay specification', () => {
			const connectionClasses = ['UserConnection', 'PostConnection', 'CategoryConnection', 'PostCategoryConnection', 'CommentConnection']

			connectionClasses.forEach((className) => {
				expect(schemaContent).toContain(`export class ${className}`)
				const connectionMatch = schemaContent.match(new RegExp(`export class ${className}[\\s\\S]*?(?=export class|$)`))
				expect(connectionMatch).not.toBeNull()

				const connectionSection = connectionMatch![0]
				expect(connectionSection).toContain('pageInfo!: PageInfo')
				expect(connectionSection).toContain('edges!:')
				expect(connectionSection).toContain('totalCount!: number')
			})
		})

		test('Edge types follow GraphQL Relay specification', () => {
			const edgeClasses = ['UserEdge', 'PostEdge', 'CategoryEdge', 'PostCategoryEdge', 'CommentEdge']

			edgeClasses.forEach((className) => {
				expect(schemaContent).toContain(`export class ${className}`)
				const edgeMatch = schemaContent.match(new RegExp(`export class ${className}[\\s\\S]*?(?=export class|$)`))
				expect(edgeMatch).not.toBeNull()

				const edgeSection = edgeMatch![0]
				expect(edgeSection).toContain('node!:')
				expect(edgeSection).toContain('cursor!: string')
			})
		})
	})

	// Advanced Filter Tests
	describe('Advanced Filter Validation', () => {
		test('All filter inputs have AND/OR logical operators', () => {
			const modelFilterClasses = ['UserFilterInput', 'PostFilterInput', 'CategoryFilterInput', 'PostCategoryFilterInput', 'CommentFilterInput']

			modelFilterClasses.forEach((className) => {
				const filterMatch = schemaContent.match(new RegExp(`export class ${className}[\\s\\S]*?(?=export class|$)`))
				expect(filterMatch).not.toBeNull()

				const filterSection = filterMatch![0]
				expect(filterSection).toContain(`AND?: ${className}[] | undefined`)
				expect(filterSection).toContain(`OR?: ${className}[] | undefined`)
			})
		})

		test('StringFilterInput has all standard operations', () => {
			const stringFilterMatch = schemaContent.match(/export class StringFilterInput[\s\S]*?(?=export class|$)/)
			expect(stringFilterMatch).not.toBeNull()

			const stringFilterSection = stringFilterMatch![0]
			const expectedOperations = ['equals', 'not', 'in', 'notIn', 'contains', 'startsWith', 'endsWith']

			expectedOperations.forEach((operation) => {
				expect(stringFilterSection).toContain(`${operation}?:`)
			})
		})

		test('NumericFilterInput has comparison operations', () => {
			const numericFilterMatch = schemaContent.match(/export class NumericFilterInput[\s\S]*?(?=export class|$)/)
			expect(numericFilterMatch).not.toBeNull()

			const numericFilterSection = numericFilterMatch![0]
			const expectedOperations = ['equals', 'not', 'gt', 'lt']

			expectedOperations.forEach((operation) => {
				expect(numericFilterSection).toContain(`${operation}?:`)
			})
		})

		test('DateTimeFilterInput has temporal operations', () => {
			const dateFilterMatch = schemaContent.match(/export class DateTimeFilterInput[\s\S]*?(?=export class|$)/)
			expect(dateFilterMatch).not.toBeNull()

			const dateFilterSection = dateFilterMatch![0]
			const expectedOperations = ['equals', 'not', 'gt', 'lt']

			expectedOperations.forEach((operation) => {
				expect(dateFilterSection).toContain(`${operation}?:`)
			})
		})
	})

	// Sort Input Validation
	describe('Advanced Sort Validation', () => {
		test('All sort inputs have proper SortDirection references', () => {
			const sortInputClasses = ['UserSortInput', 'PostSortInput', 'CategorySortInput', 'PostCategorySortInput', 'CommentSortInput']

			sortInputClasses.forEach((className) => {
				expect(schemaContent).toContain(`export class ${className}`)
				const sortMatch = schemaContent.match(new RegExp(`export class ${className}[\\s\\S]*?(?=export class|$)`))
				expect(sortMatch).not.toBeNull()

				const sortSection = sortMatch![0]
				expect(sortSection).toContain('SortDirection | undefined')
			})
		})

		test('PostSortInput includes all sortable fields', () => {
			const postSortMatch = schemaContent.match(/export class PostSortInput[\s\S]*?(?=export class|$)/)
			expect(postSortMatch).not.toBeNull()

			const postSortSection = postSortMatch![0]
			expect(postSortSection).toContain('createdAt?: SortDirection')
			expect(postSortSection).toContain('updatedAt?: SortDirection')
			expect(postSortSection).toContain('viewCount?: SortDirection')
		})

		test('UserSortInput includes timestamp fields', () => {
			const userSortMatch = schemaContent.match(/export class UserSortInput[\s\S]*?(?=export class|$)/)
			expect(userSortMatch).not.toBeNull()

			const userSortSection = userSortMatch![0]
			expect(userSortSection).toContain('createdAt?: SortDirection')
			expect(userSortSection).toContain('updatedAt?: SortDirection')
		})
	})
})
